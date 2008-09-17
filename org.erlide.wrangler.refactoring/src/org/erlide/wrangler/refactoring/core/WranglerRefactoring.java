package org.erlide.wrangler.refactoring.core;

import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IdeBackend;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.wrangler.refactoring.Activator;
import org.erlide.wrangler.refactoring.core.exception.WranglerException;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Abstract class for Wrangler refactoring integrations.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class WranglerRefactoring extends Refactoring {

	static protected OtpErlangTuple createPos(Integer line, Integer coloumn) {
		return new OtpErlangTuple(new OtpErlangInt(line), new OtpErlangInt(
				coloumn));
	}

	protected Integer coloumn, line;
	protected Change change;
	protected RefactoringParameters parameters;
	@SuppressWarnings("restriction")
	protected IdeBackend managedBackend;
	protected String newName;

	private RPCMessage message;

	/**
	 * Sole constructor. Initializes the necessary components.
	 * 
	 * @param parameters
	 *            refactoring parameters
	 */
	public WranglerRefactoring(RefactoringParameters parameters) {
		this.parameters = parameters;
		this.managedBackend = (IdeBackend) BackendManager.getDefault()
				.getIdeBackend();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ltk.core.refactoring.Refactoring#checkFinalConditions(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		RefactoringStatus rs = new RefactoringStatus();

		try {
			doRefactoring();
		} catch (WranglerException e) {
			String s = e.getLocalizedMessage();
			rs = RefactoringStatus.createFatalErrorStatus(s);
		} catch (IOException e) {
			return RefactoringStatus
					.createErrorStatus("I/O error during the refactoring:\n"
							+ e.getMessage());
		} catch (ErlangRpcException e) {
			rs = RefactoringStatus.createFatalErrorStatus(e.getMessage());
		} catch (RpcException e) {
			rs = RefactoringStatus.createFatalErrorStatus(e.getMessage());
		} catch (Exception e) {
			e.printStackTrace();
			rs = RefactoringStatus.createFatalErrorStatus(e.getMessage());
		}

		/*
		 * RPCMessage message = new RPCMessage(rpcResult); rs =
		 * message.getRefactongStatus();
		 */
		return rs;
	}

	/*
	 * 
	 * OLD VERSION protected RpcResult doRefactoring() throws IOException,
	 * CoreException, ErlangRpcException, RpcException { and then create // an
	 * abstract method to the rpc call // 1. create copier object, copy the
	 * files // 2. call the abstract method see upper // 3. create changes, with
	 * the object, described in the 1, // maybe itt will be 2 class: A) just the
	 * copier, B) interface upon the // copier and upon the fileDiff
	 * ProjectCopier pc = new ProjectCopier(parameters.getFile()); pc.doCopy();
	 * 
	 * OtpErlangList searchPathList = new OtpErlangList(new OtpErlangString(pc
	 * .getSearchPath()));
	 * 
	 * RpcResult res = sendRPC(pc.getFilePath(), searchPathList);
	 * 
	 * doOtherChanges();
	 * 
	 * change = pc.createChanges();
	 * 
	 * pc.dispose();
	 * 
	 * return res; }
	 */

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ltk.core.refactoring.Refactoring#checkInitialConditions(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		return new RefactoringStatus();
	}

	/**
	 * Converts the given <code>RpcResult</code> object to
	 * <code>RPCMessage</code> object, while checking it.
	 * 
	 * @param res
	 *            result
	 * @return the converted and checked <code>RPCMessage</code>
	 * @throws WranglerException
	 *             exception is thrown when there is an error with the
	 *             communication or the refactoring.
	 */
	protected RPCMessage convertRpcResultToRPCMessage(RpcResult res)
			throws WranglerException {
		RPCMessage m = new RPCMessage(res);
		m.checkIsOK();
		return m;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ltk.core.refactoring.Refactoring#createChange(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public Change createChange(IProgressMonitor pm) throws CoreException,
			OperationCanceledException {
		CompositeChange cChange = new CompositeChange("wrangler made changes");

		List<FileResourceChanges> fileRs = message.getResult();
		try {
			Change c;
			for (FileResourceChanges e : fileRs) {
				c = e.createChanges();
				if (c!=null) {
					cChange.add(c);
				}
			}
		} catch (IOException e) {
			Status s = new Status(Status.ERROR, Activator.PLUGIN_ID, e
					.getMessage());
			throw new CoreException(s);
		}

		Change otherChange = doOtherChanges();
		if (null != otherChange) {
			int a;
			cChange.add(otherChange);
		}

		change = cChange;
		return change;
	}

	/**
	 * If necessary classes which extend this class should override this method.
	 * 
	 * @return other <code>Change</code> objects
	 */
	protected Change doOtherChanges() {
		return null;
	}

	/**
	 * Sends the RPC to Wrangler, then checks the result and stores it.
	 * 
	 * @throws ErlangRpcException
	 * @throws RpcException
	 * @throws WranglerException
	 * @throws IOException
	 * @throws CoreException
	 */
	protected void doRefactoring() throws ErlangRpcException, RpcException,
			WranglerException, IOException, CoreException {

		String filePath = parameters.getFilePath();
		RpcResult res = sendRPC(filePath, parameters.getSearchPath());

		RPCMessage m = convertRpcResultToRPCMessage(res);
		message = m;
	}

	/**
	 * Sends an RPC to Wrangler. Extending classes must override this method.
	 * 
	 * @param filePath
	 *            OS dependent path, where the selected file is stored in the
	 *            filesystem
	 * @param searchPath
	 *            Erlang object, which contains the project directory OS
	 *            dependent path
	 * @return the result of the RPC in raw format
	 * @throws ErlangRpcException
	 * @throws RpcException
	 */
	protected abstract RpcResult sendRPC(String filePath,
			OtpErlangList searchPath) throws ErlangRpcException, RpcException;

	/**
	 * @param m
	 *            stored <code>RPCMessage</code> object
	 */
	public void setMessage(final RPCMessage m) {
		this.message = m;
	}

	/**
	 * @param newName
	 *            stored string, which is needed by the refactorings [such as
	 *            new variable name]
	 */
	public void setNewName(final String newName) {
		this.newName = newName;
	}

	public void setChange(Change c) {
		this.change = c;
	}

	public RefactoringParameters getParameters() {
		return parameters;
	}
}
