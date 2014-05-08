package org.erlide.ui.wizards;

import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.Collection;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResourceStatus;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.ide.undo.CreateFileOperation;
import org.eclipse.ui.ide.undo.CreateFolderOperation;
import org.eclipse.ui.ide.undo.CreateProjectOperation;
import org.eclipse.ui.statushandlers.IStatusAdapterConstants;
import org.eclipse.ui.statushandlers.StatusAdapter;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.core.ErlangCore;
import org.erlide.core.internal.builder.ErlangNature;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.util.StatusUtil;

import com.google.common.base.Charsets;
import com.google.common.collect.Lists;

public class ProjectCreator {

    private final String name;
    private final URI location;
    private final IProject[] referencedProjects;
    private final NewProjectData info;
    private final IAdaptable notifier;
    private final IRunnableContext context;

    public ProjectCreator(final String name, final URI location,
            final IProject[] referencedProjects, final NewProjectData info,
            final IRunnableContext context, final IAdaptable notifier) {
        this.name = name;
        this.location = location;
        this.referencedProjects = referencedProjects;
        this.info = info;
        this.notifier = notifier;
        this.context = context;
    }

    public IProject createProject() throws CoreException {
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        final IProject newProjectHandle = workspace.getRoot().getProject(name);

        if (newProjectHandle.exists()) {
            throw new CoreException(Status.OK_STATUS);
        }

        final IProjectDescription description = workspace.newProjectDescription(name);
        description.setLocationURI(location);

        // // update the referenced project if provided
        if (referencedProjects != null) {
            description.setReferencedProjects(referencedProjects);
        }

        // create the new project operation
        final IRunnableWithProgress op = new IRunnableWithProgress() {
            @Override
            public void run(final IProgressMonitor monitor)
                    throws InvocationTargetException {

                final CreateProjectOperation op1 = new CreateProjectOperation(
                        description, WizardMessages.NewProject_windowTitle);
                try {
                    // https://bugs.eclipse.org/bugs/show_bug.cgi?id=219901
                    // Making this undoable would be a bad idea
                    op1.execute(monitor, notifier);

                    newProjectHandle.open(monitor);
                    description.setNatureIds(new String[] { ErlangCore.NATURE_ID });
                    newProjectHandle.setDescription(description, null);

                    final BuilderTool builder = info.getBuilder();
                    ErlangNature.setErlangProjectBuilder(newProjectHandle, builder);
                    createBuilderConfig(builder);

                    createFolders(newProjectHandle,
                            Lists.newArrayList(info.getOutputDir()), monitor);
                    createFolders(newProjectHandle, info.getSourceDirs(), monitor);
                    createFolders(newProjectHandle, info.getIncludeDirs(), monitor);

                    createConfig(newProjectHandle, info.getConfigType(), monitor);

                    final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                            .getErlangProject(newProjectHandle);
                    erlProject.setConfigType(info.getConfigType());
                    final BuilderProperties builderProperties = new BuilderProperties();
                    builderProperties.setBuilderTool(builder);
                    builderProperties.setCompileTarget(info.getBuilderData().get(
                            "compile"));
                    builderProperties.setCleanTarget(info.getBuilderData().get("clean"));

                    erlProject.setBuilderProperties(builderProperties);
                    erlProject.setProperties(info);

                } catch (final Exception e) {
                    throw new InvocationTargetException(e);
                }
            }
        };

        // run the new project creation operation
        try {
            context.run(false, true, op);
        } catch (final InterruptedException e) {
            return null;
        } catch (final InvocationTargetException e) {
            final Throwable t = e.getTargetException();
            if (t instanceof ExecutionException && t.getCause() instanceof CoreException) {
                final CoreException cause = (CoreException) t.getCause();
                StatusAdapter status;
                if (cause.getStatus().getCode() == IResourceStatus.CASE_VARIANT_EXISTS) {
                    status = new StatusAdapter(StatusUtil.newStatus(IStatus.WARNING, NLS
                            .bind(WizardMessages.NewProject_caseVariantExistsError,
                                    newProjectHandle.getName()), cause));
                } else {
                    status = new StatusAdapter(
                            StatusUtil.newStatus(cause.getStatus().getSeverity(),
                                    WizardMessages.NewProject_errorMessage, cause));
                }
                status.setProperty(IStatusAdapterConstants.TITLE_PROPERTY,
                        WizardMessages.NewProject_errorMessage);
                StatusManager.getManager().handle(status, StatusManager.BLOCK);
            } else {
                final StatusAdapter status = new StatusAdapter(new Status(
                        IStatus.WARNING, ErlideUIPlugin.PLUGIN_ID, 0, NLS.bind(
                                WizardMessages.NewProject_internalError, t.getMessage()),
                        t));
                status.setProperty(IStatusAdapterConstants.TITLE_PROPERTY,
                        WizardMessages.NewProject_errorMessage);
                StatusManager.getManager().handle(status,
                        StatusManager.LOG | StatusManager.BLOCK);
            }
            return null;
        }

        return newProjectHandle;
    }

    private void createConfig(final IProject newProjectHandle,
            final ProjectConfigType configType, final IProgressMonitor monitor)
            throws ExecutionException {

        System.out.println("create config?? " + configType);

        switch (configType) {
        case REBAR:
        case EMAKE:
            final IFile cfg = newProjectHandle.getFile(configType.getConfigName());
            final String contents = getConfigContent(info, configType);
            final CreateFileOperation fop = new CreateFileOperation(cfg, null,
                    // TODO real encoding?
                    new ByteArrayInputStream(contents.getBytes(Charsets.ISO_8859_1)),
                    "creating file " + cfg.getName());
            fop.execute(monitor, notifier);
            break;
        case INTERNAL:
            break;
        default:
            break;
        }
    }

    private String getConfigContent(final NewProjectData info2,
            final ProjectConfigType configType) {

        // TODO get config content
        System.out.println("TO DO: get config content " + info2 + " " + configType);

        switch (configType) {
        case EMAKE:
            return "";
        case REBAR:
            return "";
        default:
            return "";
        }
    }

    private void createBuilderConfig(final BuilderTool builderTool) {

        // TODO create builder config
        System.out.println("TO DO: create builder config " + builderTool);

    }

    private void createFolders(final IProject project, final Collection<IPath> pathList,
            final IProgressMonitor monitor) throws CoreException, ExecutionException {
        if (pathList == null) {
            return;
        }
        for (final IPath path : pathList) {
            // only create in-project paths
            if (!path.isAbsolute() && !path.toString().equals(".") && !path.isEmpty()) {
                final IFolder folder = project.getFolder(path);
                createFolderHelper(folder, monitor);
            }
        }

    }

    /**
     * recursively create a folder path.
     */
    private void createFolderHelper(final IFolder folder, final IProgressMonitor monitor)
            throws CoreException, ExecutionException {
        if (!folder.exists()) {
            final IContainer parent = folder.getParent();
            if (parent instanceof IFolder && !((IFolder) parent).exists()) {
                createFolderHelper((IFolder) parent, monitor);
            }
            final CreateFolderOperation fop = new CreateFolderOperation(folder, null,
                    "creating folder " + folder.getName());
            fop.execute(monitor, notifier);
        }
    }

}
