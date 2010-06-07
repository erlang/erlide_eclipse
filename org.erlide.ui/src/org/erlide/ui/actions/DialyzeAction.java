package org.erlide.ui.actions;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.builder.DialyzerPreferences;
import org.erlide.core.builder.DialyzerUtils;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

public class DialyzeAction implements IObjectActionDelegate {

	public static class DialyzerMessageDialog extends MessageDialog {

		public static void openError(final Shell parent, final String title,
				final String message) {
			MessageDialog dialog = new DialyzerMessageDialog(parent, title,
					null, message, MessageDialog.ERROR,
					new String[] { IDialogConstants.OK_LABEL }, 0);
			dialog.open();
		}

		private final String dialogMessage;

		public DialyzerMessageDialog(final Shell parentShell,
				final String dialogTitle, final Image dialogTitleImage,
				final String dialogMessage, final int dialogImageType,
				final String[] dialogButtonLabels, final int defaultIndex) {
			super(parentShell, dialogTitle, dialogTitleImage, "",
					dialogImageType, dialogButtonLabels, defaultIndex);
			this.dialogMessage = dialogMessage;
		}

		@Override
		protected Control createCustomArea(final Composite parent) {
			Text text = new Text(parent, SWT.BORDER | SWT.V_SCROLL
					| SWT.READ_ONLY | SWT.MULTI | SWT.WRAP);
			text.setText(dialogMessage);
			GridData gd = new GridData(550, 300);
			text.setLayoutData(gd);
			return text;
		}
	}

	private final class DialyzeOperation implements IRunnableWithProgress {

		public void run(final IProgressMonitor monitor)
				throws InvocationTargetException, InterruptedException {
			final Set<IErlProject> keySet = modules.keySet();
			monitor.beginTask("Dialyzing", keySet.size());
			try {
				prefs.load();
			} catch (final BackingStoreException e) {
				ErlLogger.error(e);
			}
			DialyzerUtils.doDialyze(monitor, modules, prefs);
		}
	}

	private final Map<IErlProject, Set<IErlModule>> modules;
	private Shell shell;
	private final DialyzerPreferences prefs;

	public DialyzeAction() {
		modules = new HashMap<IErlProject, Set<IErlModule>>();
		prefs = new DialyzerPreferences();
	}

	public void setActivePart(final IAction action,
			final IWorkbenchPart targetPart) {
		shell = targetPart.getSite().getShell();
	}

	public void run(final IAction action) {
		final IRunnableWithProgress op = new DialyzeOperation();
		try {
			final IWorkbench wb = PlatformUI.getWorkbench();
			wb.getProgressService().run(true, true, op);
		} catch (final InvocationTargetException e) {
			final Throwable t = e.getCause();
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					DialyzerMessageDialog.openError(getShell(),
							"Dialyzer error", t.getMessage());
				}
			});
		} catch (final InterruptedException e) {
			ErlLogger.error(e);
		}

	}

	private Shell getShell() {
		return shell;
	}

	public void selectionChanged(final IAction action,
			final ISelection selection) {
		modules.clear();
		if (selection instanceof IStructuredSelection) {
			final IErlModel model = ErlangCore.getModel();
			final IStructuredSelection ss = (IStructuredSelection) selection;
			for (final Object i : ss.toList()) {
				try {
					model.open(null);
					if (i instanceof IResource) {
						IResource r = (IResource) i;
						DialyzerUtils.addModulesFromResource(model, r, modules);
					}
				} catch (final ErlModelException e) {
					e.printStackTrace();
				}
			}
		}
		action.setEnabled(!modules.isEmpty());
	}
}
