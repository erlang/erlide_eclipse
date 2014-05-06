package org.erlide.dialyzer.ui.handlers;

import java.lang.reflect.InvocationTargetException;
import java.util.Set;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.MultiRule;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.handlers.HandlerUtil;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendUtils;
import org.erlide.backend.api.BackendData;
import org.erlide.backend.api.IBackend;
import org.erlide.dialyzer.builder.DialyzerUtils;
import org.erlide.dialyzer.builder.DialyzerUtils.DialyzerErrorException;
import org.erlide.dialyzer.ui.Activator;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.util.ErlLogger;
import org.erlide.util.HostnameUtils;

import com.google.common.collect.Sets;

public class RunDialyzerHandler extends AbstractHandler {

    private final class DialyzeOperation extends Job {
        private final Set<IErlModule> modules;
        private final Set<IErlProject> projects;

        public DialyzeOperation(final String name, final Set<IErlModule> modules,
                final Set<IErlProject> projects) {
            super(name);
            this.modules = modules;
            this.projects = projects;
        }

        @Override
        protected IStatus run(final IProgressMonitor monitor) {
            if (!modules.isEmpty()) {
                IBackend backend = null;
                monitor.beginTask("Dialyzing", IProgressMonitor.UNKNOWN);

                try {
                    backend = createBackend();
                    DialyzerUtils.doDialyze(monitor, modules, projects, backend);
                } catch (final OperationCanceledException e) {
                    ErlLogger.debug("Dialyzer operation was canceled");
                    return Status.CANCEL_STATUS;
                } catch (final DialyzerErrorException e) {
                    return new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage());
                } catch (final InvocationTargetException e) {
                    return new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getCause()
                            .getMessage());
                } finally {
                    if (backend != null) {
                        backend.dispose();
                    }
                    monitor.done();
                }

                ErlLogger.debug("Dialyzer operation ended successfully");
            }
            return Status.OK_STATUS;
        }

        private IBackend createBackend() {
            final RuntimeInfo erlideRuntime = BackendCore.getRuntimeInfoCatalog()
                    .getErlideRuntime();
            if (erlideRuntime == null) {
                return null;
            }
            final RuntimeInfo info = new RuntimeInfo(erlideRuntime);
            final BackendData data = new BackendData(info);
            final String nodeName = BackendUtils.getErlideNodeNameTag() + "_dialyzer";
            data.setNodeName(nodeName);
            data.setDebug(false);
            data.setConsole(false);
            data.setInternal(true);
            data.setLongName(HostnameUtils.canUseLongNames());
            return BackendCore.getBackendManager().createExecutionBackend(data);
        }
    }

    private final class BuildOperation extends WorkspaceJob {
        private final Set<IErlProject> projects;

        public BuildOperation(final String name, final Set<IErlProject> projects) {
            super(name);
            this.projects = projects;
        }

        @Override
        public IStatus runInWorkspace(final IProgressMonitor monitor)
                throws CoreException {
            for (final IErlProject erlProject : projects) {
                final IProject project = erlProject.getWorkspaceProject();
                project.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, null);
            }
            return Status.OK_STATUS;
        }

        @Override
        public boolean belongsTo(final Object family) {
            return ResourcesPlugin.FAMILY_MANUAL_BUILD == family;
        }

    }

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        final ISelection selection = HandlerUtil.getCurrentSelection(event);
        final Set<IErlModule> modules = collectModulesFromSelection(selection);
        final Set<IErlProject> projects = collectProjectsFromModules(modules);

        // build
        final WorkspaceJob buildJob = new BuildOperation("Building projects", projects);
        buildJob.setUser(true);
        buildJob.setRule(ResourcesPlugin.getWorkspace().getRuleFactory().buildRule());
        buildJob.schedule();

        // run dialyzer
        final Job job = new DialyzeOperation("Running Dialyzer", modules, projects);
        final ISchedulingRule rule = createRuleForModules(modules);
        job.setRule(rule);
        job.setUser(true);
        job.schedule();
        return null;
    }

    private ISchedulingRule createRuleForModules(final Set<IErlModule> modules) {
        ISchedulingRule combinedRule = null;
        for (final IErlModule module : modules) {
            final IResource res = module.getResource();
            if (res instanceof IFile) {
                combinedRule = MultiRule.combine(combinedRule, res);
            }
        }
        return combinedRule;
    }

    private Set<IErlModule> collectModulesFromSelection(final ISelection selection) {
        final Set<IErlModule> modules = Sets.newHashSet();
        if (selection instanceof IStructuredSelection) {
            final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
            final IErlModel model = ErlangEngine.getInstance().getModel();
            try {
                model.open(null);
                for (final Object i : structuredSelection.toList()) {
                    if (i instanceof IResource) {
                        final IResource r = (IResource) i;
                        modules.addAll(DialyzerUtils.collectModulesFromResource(model, r));
                    }
                }
            } catch (final ErlModelException e) {
                ErlLogger.debug(e);
            }
        }
        return modules;
    }

    private Set<IErlProject> collectProjectsFromModules(final Set<IErlModule> modules) {
        final Set<IErlProject> projects = Sets.newHashSet();
        for (final IErlModule module : modules) {
            final IErlProject project = ErlangEngine.getInstance().getModelUtilService()
                    .getProject(module);
            projects.add(project);
        }
        return projects;
    }

}
