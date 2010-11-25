package org.erlide.ui.actions;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.MultiRule;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.builder.DialyzerPreferences;
import org.erlide.core.builder.DialyzerUtils;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Sets;

public class DialyzeAction implements IObjectActionDelegate {

    public static class DialyzerMessageDialog extends MessageDialog {

        public static void openError(final Shell parent, final String title,
                final String message) {
            final MessageDialog dialog = new DialyzerMessageDialog(parent,
                    title, null, message, MessageDialog.ERROR,
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
            final Text text = new Text(parent, SWT.BORDER | SWT.V_SCROLL
                    | SWT.READ_ONLY | SWT.MULTI | SWT.WRAP);
            text.setText(dialogMessage);
            final GridData gd = new GridData(550, 300);
            text.setLayoutData(gd);
            return text;
        }
    }

    private final class DialyzeOperation extends Job {

        public DialyzeOperation(final String name) {
            super(name);
        }

        IStatus newErrorStatus(final Throwable throwable) {
            return new Status(IStatus.ERROR, ErlangPlugin.PLUGIN_ID,
                    throwable.getMessage());
        }

        @Override
        protected IStatus run(final IProgressMonitor monitor) {
            final Set<IErlProject> keySet = modules.keySet();
            monitor.beginTask("Dialyzing", keySet.size());
            try {
                prefs.load();
                DialyzerUtils.doDialyze(monitor, modules, prefs);
            } catch (final BackingStoreException e) {
                return newErrorStatus(e);
            } catch (final InvocationTargetException e) {
                return newErrorStatus(e.getCause());
            }
            return Status.OK_STATUS;
        }
    }

    private final Map<IErlProject, Set<IErlModule>> modules;
    private final DialyzerPreferences prefs;

    public DialyzeAction() {
        modules = new HashMap<IErlProject, Set<IErlModule>>();
        prefs = new DialyzerPreferences();
    }

    public void setActivePart(final IAction action,
            final IWorkbenchPart targetPart) {
    }

    public ISchedulingRule createRule(final Collection<IFile> files) {
        ISchedulingRule combinedRule = null;
        for (final IFile file : files) {
            combinedRule = MultiRule.combine(file, combinedRule);
        }
        return combinedRule;
    }

    public void run(final IAction action) {
        final Job job = new DialyzeOperation("Running Dialyzer");
        final Set<IFile> files = collectFiles();
        final ISchedulingRule rule = createRule(files);
        job.setRule(rule);
        job.setUser(true);
        job.schedule();
    }

    private Set<IFile> collectFiles() {
        final Set<IFile> result = Sets.newHashSet();
        for (final Set<IErlModule> set : modules.values()) {
            for (final IErlModule module : set) {
                final IResource resource = module.getResource();
                if (resource instanceof IFile) {
                    result.add((IFile) resource);
                }
            }
        }
        return result;
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
                        final IResource r = (IResource) i;
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
