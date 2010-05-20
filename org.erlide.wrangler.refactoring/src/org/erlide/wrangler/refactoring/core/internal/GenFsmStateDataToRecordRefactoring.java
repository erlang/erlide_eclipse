package org.erlide.wrangler.refactoring.core.internal;

import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.internal.StateDataToRecordRpcMessage;
import org.erlide.wrangler.refactoring.core.QuickCheckStateRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangBoolean;

/**
 * Integration of the gen_fsm state data to record refactoring
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class GenFsmStateDataToRecordRefactoring extends
		QuickCheckStateRefactoring {

	@Override
	public String getName() {
		return "gen_fsm State data to record";
	}

	@Override
	protected StateDataToRecordRpcMessage runFirst(IErlMemberSelection sel) {
		IRefactoringRpcMessage parser = new StateDataToRecordRpcMessage();

		return (StateDataToRecordRpcMessage) WranglerBackendManager
				.getRefactoringBackend().callWithParser(parser,
						"gen_fsm_to_record_eclipse", "sxi", sel.getFilePath(),
						sel.getSearchPath(), GlobalParameters.getTabWidth());
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection sel) {

		return (IRefactoringRpcMessage) WranglerBackendManager
				.getRefactoringBackend().call("gen_fsm_to_record_1_eclipse",
						"ssxxxxi", sel.getFilePath(), getRecordName(),
						getFieldsName(), stateFuns,
						new OtpErlangBoolean(fieldCount > 1),
						sel.getSearchPath(), GlobalParameters.getTabWidth());
	}

}
