package org.erlide.ui.handlers;

import java.io.File;
import java.util.Set;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.MultiRule;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.builder.DialyzerPreferences;
import org.erlide.core.builder.DialyzerUtils;
import org.erlide.core.builder.DialyzerUtils.DialyzerErrorException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.ErlideBackend;
import org.osgi.service.prefs.BackingStoreException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Sets;

import erlang.ErlideDialyze;

public class CheckDialyzerPltFileHandler extends AbstractHandler implements
        IHandler {

    public static final String COMMAND_ID = "org.erlide.ui.command.checkDialyzerPLTFile";

    Set<IProject> projects = Sets.newHashSet();

    private final class UpdateDialyzerPLTFileOperation extends Job {

        public UpdateDialyzerPLTFileOperation(final String name) {
            super(name);
        }

        IStatus newErrorStatus(final Throwable throwable) {
            return new Status(IStatus.ERROR, ErlangPlugin.PLUGIN_ID,
                    throwable.getMessage());
        }

        @Override
        protected IStatus run(final IProgressMonitor monitor) {
            final BackendManager backendManager = ErlangCore
                    .getBackendManager();
            try {
                if (projects.isEmpty()) {
                    final DialyzerPreferences preferences = new DialyzerPreferences();
                    final ErlideBackend backend = backendManager
                            .getIdeBackend();
                    checkPlt(preferences, monitor, backend);
                } else {
                    for (final IProject project : projects) {
                        final Backend backend = backendManager
                                .getBuildBackend(project);
                        final DialyzerPreferences preferences = new DialyzerPreferences(
                                project);
                        checkPlt(preferences, monitor, backend);
                    }
                }
            } catch (final BackendException e) {
                return newErrorStatus(e);
            } catch (final DialyzerErrorException e) {
                return newErrorStatus(e);
            } catch (final BackingStoreException e) {
                return newErrorStatus(e);
            } finally {
                monitor.done();
            }
            return Status.OK_STATUS;
        }

        private void checkPlt(final DialyzerPreferences preferences,
                final IProgressMonitor monitor, final Backend backend)
                throws DialyzerErrorException, BackingStoreException {
            try {
                preferences.load();
                final String pltPath = preferences.getPltPath();
                monitor.subTask("Checking PLT file " + pltPath);
                final OtpErlangObject result = ErlideDialyze.checkPlt(backend,
                        pltPath);
                DialyzerUtils.checkDialyzeError(result);
            } finally {
                monitor.worked(1);
            }
        }
    }

    public ISchedulingRule createRule(final Set<IProject> theProjects) {
        ISchedulingRule combinedRule = null;
        for (final IProject project : theProjects) {
            combinedRule = MultiRule.combine(project, combinedRule);
        }
        return combinedRule;
    }

    public Object execute(final ExecutionEvent event) throws ExecutionException {
        final Job job = new UpdateDialyzerPLTFileOperation("Checking PLT file");
        final ISchedulingRule rule = createRule(projects);
        job.setRule(rule);
        job.setUser(true);
        job.setSystem(false);
        job.schedule();
        return null;
    }

    @Override
    public void setEnabled(final Object evaluationContext) {
        boolean enabled = false;
        final IEvaluationContext ec = (IEvaluationContext) evaluationContext;
        final Object selection = ec.getVariable("selection");
        if (selection instanceof IStructuredSelection) {
            final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
            for (final Object o : structuredSelection.toList()) {
                if (o instanceof IProject) {
                    final IProject project = (IProject) o;
                    if (ErlideUtil.hasErlangNature(project)) {
                        final DialyzerPreferences preferences = new DialyzerPreferences(
                                project);
                        if (preferences.hasOptionsAtLowestScope()) {
                            if (validPlt(preferences)) {
                                projects.add(project);
                            }
                        }
                    }
                }
            }
            enabled = !projects.isEmpty();
        } else {
            final DialyzerPreferences preferences = new DialyzerPreferences();
            enabled = validPlt(preferences);
        }
        setBaseEnabled(enabled);
    }

    private boolean validPlt(final DialyzerPreferences preferences) {
        try {
            preferences.load();
            final File f = new File(preferences.getPltPath());
            return f.exists();
        } catch (final BackingStoreException e) {
            return false;
        }
    }

}
