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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.PlatformUI;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.abstratt.graphviz.GraphViz;
import com.ericsson.otp.erlang.OtpErlangBoolean;

/**
 * This class is for handling commands which are from wrangler code inspection
 * functionality, and result a graph.
 * 
 * @author Gyorgy Orosz
 * 
 */
public class GraphResultingInspectionHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		String actionId = event.getCommand().getId();
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
				.getActiveEditor().setFocus();
		GlobalParameters.setSelection(PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage().getSelection());
		try {
			File tmpFile = File.createTempFile("wrangler_graph_", ".dot");
			tmpFile.deleteOnExit();

			IErlSelection wranglerSelection = GlobalParameters
					.getWranglerSelection();

			if (actionId
					.equals("org.erlide.wrangler.refactoring.codeinspection.cyclicdependencies")) {
				runInspection("Cyclic module dependency",
						"There is no cyclic dependent modules in the project!",
						tmpFile, "cyclic_dependent_modules", "ssx", tmpFile
								.getAbsolutePath(), wranglerSelection
								.getSearchPath(), new OtpErlangBoolean(true));
			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.codeinspection.generatefunctioncallgraph")) {
				runInspection("Function callgraph",
						"There is no dependent functions in the module!",
						tmpFile, "gen_function_callgraph", "sss", tmpFile
								.getAbsolutePath(), wranglerSelection
								.getFilePath(), wranglerSelection
								.getSearchPath());

			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.codeinspection.generatemodulegraph")) {
				runInspection("Module dependency graph",
						"There is no dependent modules in the project!",
						tmpFile, "gen_module_graph", "ssx", tmpFile
								.getAbsolutePath(), wranglerSelection
								.getSearchPath(), new OtpErlangBoolean(true));

			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.codeinspection.improperdependecies")) {
				runInspection("Improper module dependencies",
						"There is no improper module dependecies!", tmpFile,
						"improper_inter_module_calls", "ss", tmpFile
								.getAbsolutePath(), wranglerSelection
								.getSearchPath());

			}

		} catch (Exception e) {
			e.printStackTrace();
		}
		return event;
	}

	/**
	 * Run code inspection function, and shows the result in the workbench
	 * 
	 * @param viewtTitle
	 *            title of the view
	 * @param noResultMessage
	 *            if there is no result, this message will be displayed
	 * @param tmpFile
	 *            temp .dot file
	 * @param functionName
	 *            function to call
	 * @param signature
	 *            parameters signature
	 * @param parameters
	 *            function parameters
	 * 
	 */
	protected void runInspection(String viewtTitle, String noResultMessage,
			File tmpFile, String functionName, String signature,
			Object... parameters) {
		try {
			CodeInspectionViewsManager
					.hideView(CodeInspectionViewsManager.GRAPH_VIEW);
			FileInputStream fis = new FileInputStream(tmpFile);
			Boolean b = WranglerBackendManager.getRefactoringBackend()
					.callSimpleInspection(functionName, signature, parameters);
			if (b) {
				if (fis.available() > 0) {

					Image img = GraphViz.load(fis, "png", new Point(0, 0));
					CodeInspectionViewsManager.showDotImage(img, viewtTitle,
							tmpFile);
				} else
					MessageDialog.openInformation(GlobalParameters.getEditor()
							.getSite().getShell(), viewtTitle, noResultMessage);

			} else {
				MessageDialog.openError(GlobalParameters.getEditor().getSite()
						.getShell(), "Internal error",
						"Internal error occured. Please report it!");
			}
		} catch (IOException e) {
			e.printStackTrace();
		} catch (CoreException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}

	}
}
