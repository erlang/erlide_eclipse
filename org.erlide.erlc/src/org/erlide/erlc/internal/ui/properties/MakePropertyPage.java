package org.erlide.erlc.internal.ui.properties;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.preference.IPreferencePageContainer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PropertyPage;
import org.erlide.erlc.ErlideErlcPlugin;
import org.erlide.erlc.internal.ui.MakeProjectOptionBlock;
import org.erlide.erlc.ui.dialogs.IErlOptionContainer;

public class MakePropertyPage extends PropertyPage implements
		IErlOptionContainer {

	MakeProjectOptionBlock fOptionBlock;

	private static final String MSG_CLOSEDPROJECT = "MakeProjectPropertyPage.closedproject"; //$NON-NLS-1$

	public MakePropertyPage() {
		super();
		fOptionBlock = new MakeProjectOptionBlock();
	}

	@Override
	public void setContainer(IPreferencePageContainer preferencePageContainer) {
		super.setContainer(preferencePageContainer);
		fOptionBlock.setOptionContainer(this);
	}

	@Override
	protected Control createContents(Composite parent) {
		final Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new FillLayout());

		final IProject project = getProject();
		if (!project.isOpen()) {
			contentForClosedProject(composite);
		} else {
			contentForErlProject(composite);
		}

		return composite;
	}

	private void contentForErlProject(Composite parent) {
		fOptionBlock.createContents(parent);
		// WorkbenchHelp.setHelp(parent,
		// ICMakeHelpContextIds.PROJECT_PROPERTY_PAGE);
	}

	private void contentForClosedProject(Composite parent) {
		final Label label = new Label(parent, SWT.LEFT);
		label.setText(ErlideErlcPlugin.getResourceString(MSG_CLOSEDPROJECT));
		label.setFont(parent.getFont());

		noDefaultAndApplyButton();
	}

	@Override
	public boolean performOk() {
		final IRunnableWithProgress runnable = new IRunnableWithProgress() {

			public void run(IProgressMonitor monitor) {
				fOptionBlock.performApply(monitor);
			}
		};
		try {
			PlatformUI.getWorkbench().getProgressService().runInUI(
					PlatformUI.getWorkbench().getProgressService(), runnable,
					ResourcesPlugin.getWorkspace().getRoot());
		} catch (final InvocationTargetException e) {
			final Throwable e1 = e.getTargetException();
			ErlideErlcPlugin
					.errorDialog(
							getShell(),
							ErlideErlcPlugin
									.getResourceString("MakeProjectPropertyPage.internalError"), e1.toString(), e1, false); //$NON-NLS-1$
			return false;
		} catch (final InterruptedException e) {
			// cancelled
			return false;
		}
		return true;
	}

	public IProject getProject() {
		final Object element = getElement();
		if (element instanceof IProject) {
			return (IProject) element;
		}
		return null;
	}

	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		fOptionBlock.setVisible(visible);
	}

	public void updateContainer() {
		fOptionBlock.update();
		setValid(fOptionBlock.isValid());
		setErrorMessage(fOptionBlock.getErrorMessage());
	}

	@Override
	protected void performDefaults() {
		fOptionBlock.performDefaults();
		super.performDefaults();
	}

	@Override
	public boolean isValid() {
		updateContainer();
		return super.isValid();
	}

	public Preferences getPreferences() {
		return ErlideErlcPlugin.getDefault().getPluginPreferences();
	}

}