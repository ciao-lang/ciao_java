import CiaoJava.*;
import java.util.*;

/**
 * ABSTRACT
 * Java to ciao interface example.
 **/
public class j2pl_test {

  // Prolog process.
  private static PLConnection plServer = null;

  /**
   * Start method. 
   **/
  public static void main(String argv[]) {
    try {
	plServer = new PLConnection(argv);
    } catch (Exception e) {
      System.err.println("ERROR: Problems starting java server: " + e);
      System.exit(1);
    }

    char[] sol1 = {5,7,2,6,3,1,4,8};
    char[] sol2 = {4,7,5,2,6,1,3,8};
    PLVariable plX = new PLVariable();
    PLTerm[] args = {new PLInteger(8), plX};
    PLStructure strGoal = new PLStructure("queens", args);

    PLGoal goal = new PLGoal(plServer,strGoal);
    try {
      goal.useModule("queens");
      goal.query();
      goal.nextSolution();
      if (!Arrays.equals(((PLString)plX.getBinding()).getValue().toCharArray(),sol1)) {
	  System.err.println("ERROR: Problems getting first solution.");
      }
      else {
	  goal.nextSolution();
	  if (Arrays.equals(((PLString)plX.getBinding()).getValue().toCharArray(),sol2)) 
	      System.err.println("Java to Prolog test succeeded");
	  else {
	      System.err.println("ERROR: Problems getting second solution.");
	  }
      }
      goal.terminate();
      plServer.stop();
    } catch (Exception e) {
      System.err.println("ERROR: Problems launching goal: " + e);
      System.exit(1);
    }
    System.exit(0);
  }
}

