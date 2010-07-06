/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.codeinspection;

import java.util.ArrayList;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class SimpleCodeInspectionHandler extends AbstractHandler implements
		IHandler {
	static protected final String LARGE_MODULES_VIEW_ID = "largemodules";
	static protected final String DEPENECIES_1_VIEW_ID = "dependencies1";
	static protected final String DEPENECIES_2_VIEW_ID = "dependencies2";

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		String actionId = event.getCommand().getId();
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
				.getActiveEditor().setFocus();
		GlobalParameters.setSelection(PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage().getSelection());

		IErlSelection wranglerSelection = GlobalParameters
				.getWranglerSelection();
		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
				.getShell();

		if (actionId
				.equals("org.erlide.wrangler.refactoring.codeinspection.largemodules")) {
			handleLargeModulesCall(wranglerSelection, shell);

		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.codeinspection.dependencies")) {
			handleDepenenciesCall(wranglerSelection, shell);
		}

		return event;
	}

	private void handleDepenenciesCall(IErlSelection wranglerSelection,
			Shell shell) {
		// hiding the views
		CodeInspectionViewsManager.hideView(
				CodeInspectionViewsManager.CODE_INSPECTION_VIEW,
				DEPENECIES_1_VIEW_ID);
		CodeInspectionViewsManager.hideView(
				CodeInspectionViewsManager.CODE_INSPECTION_VIEW,
				DEPENECIES_2_VIEW_ID);

		// run the rpc
		try {
			RpcResult res = WranglerBackendManager.getRefactoringBackend()
					.callInspection("dependencies_of_a_module_eclipse", "sx",
							wranglerSelection.getFilePath(),
							wranglerSelection.getSearchPath());

			ArrayList<IErlElement> modules1 = new ArrayList<IErlElement>();
			ArrayList<IErlElement> modules2 = new ArrayList<IErlElement>();

			OtpErlangObject obj = res.getValue();
			OtpErlangTuple restuple = (OtpErlangTuple) obj;
			OtpErlangAtom resindicator = (OtpErlangAtom) restuple.elementAt(0);
			if (resindicator.atomValue().equals("ok")) {
				OtpErlangTuple listtuple = (OtpErlangTuple) restuple
						.elementAt(1);
				OtpErlangList modList1 = (OtpErlangList) listtuple.elementAt(0);
				OtpErlangList modList2 = (OtpErlangList) listtuple.elementAt(1);
				modules1 = createErlMOduleList(modList1);
				modules2 = createErlMOduleList(modList2);
			} else {
				OtpErlangString s = (OtpErlangString) restuple.elementAt(1);
				MessageDialog.openError(shell, "Error", s.stringValue());
				return;
			}

			if (!modules1.isEmpty())
				CodeInspectionViewsManager.showErlElements(
						"Modules which depends on "
								+ wranglerSelection.getErlElement().getModule()
										.getName(), modules1,
						DEPENECIES_1_VIEW_ID);
			if (!modules2.isEmpty())
				CodeInspectionViewsManager.showErlElements("Modules, on which "
						+ wranglerSelection.getErlElement().getModule()
								.getName() + " depends", modules2,
						DEPENECIES_2_VIEW_ID);
			else
				MessageDialog
						.openInformation(shell, "No result",
								"There is no large module with the specified parameter!");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private ArrayList<IErlElement> createErlMOduleList(OtpErlangList modList) {
		ArrayList<IErlElement> modules = new ArrayList<IErlElement>();
		for (int i = 0; i < modList.arity(); ++i) {
			String name = "";
			if (modList.elementAt(i) instanceof OtpErlangString) {
				OtpErlangString element = (OtpErlangString) modList
						.elementAt(i);
				name = element.stringValue();
			} else if (modList.elementAt(i) instanceof OtpErlangAtom) {
				OtpErlangAtom atom = (OtpErlangAtom) modList.elementAt(i);
				name = atom.atomValue();
			}
			String[] modNameParts = name.split("/");
			IErlModule m = ErlangCore.getModel().findModule(
					modNameParts[modNameParts.length - 1]);
			modules.add(m);
		}
		return modules;
	}

	private void handleLargeModulesCall(IErlSelection wranglerSelection,
			Shell shell) {
		CodeInspectionViewsManager.hideView(
				CodeInspectionViewsManager.CODE_INSPECTION_VIEW,
				LARGE_MODULES_VIEW_ID);

		InputDialog dialog = new InputDialog(shell, "Lines of a large module",
				"Lines of a large module:", "", new IInputValidator() {

					@Override
					public String isValid(String newText) {
						try {
							Integer.parseInt(newText);
							return null;
						} catch (Exception e) {
							return "Please type an integer!";
						}

					}
				});
		int ret = dialog.open();
		if (ret == dialog.CANCEL)
			return;
		int lines = Integer.parseInt(dialog.getValue());
		RpcResult res = WranglerBackendManager.getRefactoringBackend()
				.callInspection("large_modules_eclipse", "ixi", lines,
						wranglerSelection.getSearchPath(),
						GlobalParameters.getTabWidth());

		ArrayList<IErlElement> modules = new ArrayList<IErlElement>();
		try {
			OtpErlangObject obj = res.getValue();
			OtpErlangTuple restuple = (OtpErlangTuple) obj;
			OtpErlangAtom resindicator = (OtpErlangAtom) restuple.elementAt(0);
			if (resindicator.atomValue().equals("ok")) {

				OtpErlangList modList = (OtpErlangList) restuple.elementAt(1);
				modules = createErlMOduleList(modList);
			} else {
				OtpErlangString s = (OtpErlangString) restuple.elementAt(1);
				MessageDialog.openError(shell, "Error", s.stringValue());
				return;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (!modules.isEmpty())
			CodeInspectionViewsManager.showErlElements("Large modules",
					modules, LARGE_MODULES_VIEW_ID);
		else
			MessageDialog.openInformation(shell, "No result",
					"There is no large module with the specified parameter!");
	}
}
