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
import org.erlide.wrangler.refactoring.codeinspection.GraphViewManager;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.abstratt.graphviz.GraphViz;
import com.ericsson.otp.erlang.OtpErlangBoolean;

public class GraphResultingInspectionHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		String actionId = event.getCommand().getId();
		GlobalParameters.setSelection(PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage().getSelection());
		try {
			File tmpFile = File.createTempFile("wrangler_graph_", ".dot");

			IErlSelection wranglerSelection = GlobalParameters
					.getWranglerSelection();

			if (actionId
					.equals("org.erlide.wrangler.refactoring.codeinspection.cyclicdependencies")) {
				runInspection("Cyclic module dependencies",
						"There is no cyclic dependent modules in the project!",
						tmpFile, "cyclic_dependent_modules", "ssx", tmpFile
								.getAbsolutePath(), wranglerSelection
								.getSearchPath(), new OtpErlangBoolean(true));
			} else if (actionId.equals("")) {

			} else if (actionId.equals("")) {

			} else if (actionId.equals("")) {

			} else if (actionId.equals("")) {

			}

		} catch (Exception e) {
			e.printStackTrace();
		}
		return event;
	}

	protected void runInspection(String viewtTitle, String noResultMessage,
			File tmpFile, String functionName, String signature,
			Object... parameters) {
		try {
			GraphViewManager.hideView();
			FileInputStream fis = new FileInputStream(tmpFile);
			Boolean b = WranglerBackendManager.getRefactoringBackend()
					.callSimpleInspection(functionName, signature, parameters);
			if (b) {
				if (fis.available() > 0) {

					Image img = GraphViz.load(fis, "png", new Point(0, 0));
					GraphViewManager.setImage(img, viewtTitle);
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
