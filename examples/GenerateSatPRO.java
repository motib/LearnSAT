/*
                       GenerateSatPRO

GenerateSatPro is a Java program that generates propositional formulas
sets of Tseitin clauses on the complete bipartite graph Kn,n.
For an explanation of the construction of Tseitin clauses,
see Section 4.5 of M. Ben-Ari, Mathematical Logic for Computer Science,
Springer, 2012.
The formulas are generated in CNF for use with the LearnSAT SAT solver.

Usage
  GenerateSatPRO n [k]
    n is the size of the graph Kn,n
    k is the number of satisfiable formulas generated
 
    Copyright 2012-7 by Mordechai (Moti) Ben-Ari. GNU GPL

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.
  This program is distributed in the hope that it will be useful
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
  02111-1307, USA.
*/



import java.io.*;
public class GenerateSatPRO {
  // The n of the bipartite graph Kn,n
	static private int n;

	// Compute and store the value 2^n
  // This is the number of edges in Kn,n since each
  //   of n nodes on the left is connected to n nodes on the right
	static private int twon;

	// Literal counter
	static private int literals;
	// Literal to change to make the formula satisfiable
	static private int change;

  // Write the program for the formula
	static private PrintWriter programWriter;


  // Emit a line to a file
	static void emit(String s) {
	  programWriter.println(s);
	}

  // Generate variable names p1, p2, ...
	static String ch(int i) { return "p" + i; };

  // Decide if i has an odd number of bits
	static boolean oddBits(int i) {
		boolean odd = false;
		while (i != 0) {
			if ((i & 1) == 1) odd = !odd;
			i = i >> 1;
		}
		return odd;
	}
	
  // Return "!" or " "
  //   bits is the bit vector
  //   negate if the ith bit is on
	static String negate(int i, int bits) {
	  boolean neg = (bits & (1 << (i-1))) > 0;
	  if (change == literals) neg = !neg;
		return neg ? "~" : " ";
	}

	// Generate a clause with n literals
	//
	//   On the left side of the bipartite graph,
	//   i is the i'th node and the edges are numbered:
	//     0,1,...,n-1,
	//     n,n+1,...,2n-1,
	//     ...
	//     n*n-n,n*n-(n-1),...,n*n-1
	//
	//   On the right side of the bipartite graph,
	//   i is the (i-n)'th node and the edges are numbered:
	//     0,n,2*n,...,(n-1)*n,
	//     ...
	//     n-1,(n-1)+n,(n-1)*2*n,...,(n-1)+(n-1)*n
	static void clause(boolean last, int n, int i, int k, boolean left) {
		String s = "\t[";
		for (int j = 0; j < n; j++) {
		  literals++;
			s = s + 
		      negate(j+1, k)           + // decide if negated or not
		      ch(left ? n*i+j : n*j+i) + // left side or right side
		      (j==n-1 ? "" : ", ");    // add disjunction except at end
		}
		s = s + "]" + (last ? "" : ",");
		emit(s);
	}
	
  // Generate a Prolog program for graph Kn,n
  //   r is the r'th random file, 0 for unsatisfiable
	static void writeSat(int r) {
    // Create the file satn-r.pro
    String name = "sat" + n + "-" + r + ".pro";
		try {
			programWriter = 
				new PrintWriter(new FileWriter(name));
		}
    catch (IOException e) {
      System.out.println("Cannot open file"); 
    }

    // Display a status message
    System.out.println("Generating file " + name + ", " +
      (r == 0 ? "unsatisfiable" : "changed literal " + change) );

    //  Prepare the printf statements in the program
    //    printf("p1 p2 ... ");
    //    printf("%d %d ...\n, p1, p2, ...);
    //
		String t0 = "";   // List of variables for printf (and declarations)
    String t1 = "";   // Print the names of the variables
    String t2 = " ";  // Format specifier for printf
		for (int i = 0; i < n*n; i++) {
			t0 = t0 + ", " + ch(i);
			t1 = t1 + ch(i) + " ";
			t2 = t2 + "%d  ";
		}

    // Emit program header and declarations
		emit(":- ensure_loaded([negation,display,dpll]).\n");
		emit("\tk" + n + n + " :-");  // Delete leading comma
		emit("\tdpll(\n\t[");
		
		// A bit vector for negating individual literals 
		int bits;
    // Parity of the node label. The first node is 0, the rest are 1		
		boolean parity;
		// Flag for last clause so as not to write comma
		boolean last;
    // Reset literal counter
    literals = 0;

		// Loop for generating clauses at each of the 2*n nodes:
		//   n on the left and n on the right
		for (int i = 0; i < 2*n; i++) {
			parity = i == 0;
			bits = 0;
      // Since each node is connected to n others,
      //   there are 2^n possible clauses on the literals labeling
      //   the edges, but only half are of the correct parity.
			for (int k = 0; k < twon/2 ; k++) {
			  // Skip bit vectors where there are an odd number of bits
			  //   and odd parity, or an even number of bits and even parity;
			  //   use only bit vectors where these are different parity
			  // Count bits as 1,2,3,...,2*n-1,0
				do {
				  bits = (bits+1) % twon;
				} 
				while (oddBits(bits) == parity);
				// Generate a clause for the n'th node
				//   If i < n, this is the left side of the bipartite graph
        last = (i == 2*n-1) && (k == twon/2 -1);
				clause(last, n, i<n ? i : i-n, bits, i<n);
			}
		}

		emit("\t], _).");
		programWriter.close();
	}

  // Usage notice and terminate
  public static void Usage() {
    System.out.println(
      "GenerateSatPRO n [k]\n" +
      "  For the complete bipartite graph Kn,n,\n" +
      "    generates a Prolog program in the file satn-0.pro\n" +
      "    for the unsatisfiable set of Tseitin clauses.\n" +
      "  Optionally, programs for k random satisfiable formulas\n" +
      "    are generated in files satn-i.pro, 1<=i<=k.\n");
    System.exit(1);
  }

  // Generate clauses for various Kn
	public static void main(String[] args) {
    // Number of literals in the formula
    int maxLiterals = 0;
    // Number of random satisfiable formulas
    int numRandom = 0;

	  if (args.length < 1 || args.length > 3)
	    Usage();
	  
	  try {
	    n = Integer.parseInt(args[0]);
      // Compute 2^n, 2 to the power n
      twon = 1;
      for (int i = 0; i < n; i++) twon *= 2;
	    maxLiterals = 2*n * (twon/2) * n;

      // Number of programs for satisfiable formulas
	    if (args.length >= 2)
        numRandom = Integer.parseInt(args[1]);
	  }
	  catch (NumberFormatException e) {
	    Usage();
	  }

    // Generate the unsatisfiable formula
    change = 0;
	  writeSat(0);

    // Generate the satisfiable formulas
    if (args.length >= 2)
      for (int i=0; i < numRandom; i++) {
        change = ((int) (Math.random()*maxLiterals)) + 1;
        writeSat(i+1);
      } 

	}
}
