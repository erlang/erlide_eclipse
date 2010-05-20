package org.erlide.wrangler.refactoring.backend;

import java.util.ArrayList;

/**
 * Interface for handling RpcMessages which contains information about a
 * Wrangler refactoring
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public interface IRefactoringRpcMessage extends IRpcMessage {
	/**
	 * Successful refactorings contains source file modification, which are
	 * represented with filename-source code pairs
	 * 
	 * @return changed files list
	 */
	public ArrayList<ChangedFile> getRefactoringChangeset();

}
