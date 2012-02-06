package org.erlide.ui.handlers;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.MultiRule;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.erlide.core.ErlangCore;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.services.builder.DialyzerUtils;
import org.erlide.jinterface.ErlLogger;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public class RunDialyzerHandler extends AbstractHandler implements IHandler {

    final Map<IErlProject, Set<IErlModule>> modules = Maps.newHashMap();

    public static class DialyzerMessageDialog extends MessageDialog {

        private static final int MAX_MESSAGE_LENGTH = 32767;

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
                String dialogMessage, final int dialogImageType,
                final String[] dialogButtonLabels, final int defaultIndex) {
            super(parentShell, dialogTitle, dialogTitleImage, "",
                    dialogImageType, dialogButtonLabels, defaultIndex);
            ErlLogger.error("dialyzer error:\n" + dialogMessage);
            if (dialogMessage.length() > MAX_MESSAGE_LENGTH) {
                dialogMessage = dialogMessage.substring(0, MAX_MESSAGE_LENGTH);
            }
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
            return new Status(IStatus.ERROR, ErlangCore.PLUGIN_ID,
                    throwable.getMessage());
        }

        @Override
        protected IStatus run(final IProgressMonitor monitor) {
            final Set<IErlProject> keySet = modules.keySet();
            monitor.beginTask("Dialyzing", keySet.size());
            try {
                DialyzerUtils.doDialyze(monitor, modules);
            } catch (final InvocationTargetException e) {
                return newErrorStatus(e.getCause());
            }
            return Status.OK_STATUS;
        }
    }

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        final Job job = new DialyzeOperation("Running Dialyzer");
        final Set<IFile> files = collectFiles();
        final ISchedulingRule rule = createRule(files);
        job.setRule(rule);
        job.setUser(true);
        job.schedule();
        return null;
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

    public ISchedulingRule createRule(final Collection<IFile> files) {
        ISchedulingRule combinedRule = null;
        for (final IFile file : files) {
            combinedRule = MultiRule.combine(file, combinedRule);
        }
        return combinedRule;
    }

    @Override
    public void setEnabled(final Object evaluationContext) {
        final IEvaluationContext ec = (IEvaluationContext) evaluationContext;
        final Object selection = ec.getVariable("selection");
        if (selection instanceof IStructuredSelection) {
            final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
            modules.clear();
            final IErlModel model = ErlModelManager.getErlangModel();
            try {
                model.open(null);
                for (final Object i : structuredSelection.toList()) {
                    if (i instanceof IResource) {
                        final IResource r = (IResource) i;
                        DialyzerUtils.addModulesFromResource(model, r, modules);
                    }
                }
            } catch (final ErlModelException e1) {
                e1.printStackTrace();
            }

        }
        setBaseEnabled(!modules.isEmpty());
    }

}
