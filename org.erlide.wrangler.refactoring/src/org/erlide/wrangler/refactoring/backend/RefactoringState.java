package org.erlide.wrangler.refactoring.backend;

/**
 * State of a Wrangler Refactoring.
 * 
 * The class does not intend to cover all the cases, just thise which are
 * essential.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public enum RefactoringState {
	OK, ERROR, WARNING, QUESTION, MULTI_INSTANCES, UNKNOWN_SIDE_EFFECT, MORE_THAN_ONE_CLAUSE;
}
