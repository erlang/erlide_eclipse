/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.internal;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.concurrent.TimeUnit;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.text.templates.ContextTypeRegistry;
import org.eclipse.jface.text.templates.persistence.TemplateStore;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.templates.ContributionContextTypeRegistry;
import org.eclipse.ui.editors.text.templates.ContributionTemplateStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.wb.swt.SWTResourceManager;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendHelper;
import org.erlide.backend.IBackend;
import org.erlide.core.ErlangStatus;
import org.erlide.debug.ui.model.ErlangDebuggerBackendListener;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.ErlideImage;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.console.ErlConsoleManager;
import org.erlide.ui.console.ErlangConsolePage;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.actions.ClearCacheAction;
import org.erlide.ui.editors.erl.completion.ErlangContextType;
import org.erlide.ui.internal.folding.ErlangFoldingStructureProviderRegistry;
import org.erlide.ui.templates.ErlangSourceContextTypeModule;
import org.erlide.ui.templates.ErlangSourceContextTypeModuleElement;
import org.erlide.ui.templates.ErlideContributionTemplateStore;
import org.erlide.ui.util.BackendManagerPopup;
import org.erlide.ui.util.IContextMenuConstants;
import org.erlide.ui.util.ImageDescriptorRegistry;
import org.erlide.ui.util.ProblemMarkerManager;
import org.erlide.utils.SystemUtils;
import org.osgi.framework.BundleContext;

import com.google.common.collect.Lists;

/**
 * The main plugin class to be used in the desktop.
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class ErlideUIPlugin extends AbstractUIPlugin {

    /**
     * The plugin id
     */
    public static final String PLUGIN_ID = "org.erlide.ui";

    /**
     * The shared instance.
     */
    private static volatile ErlideUIPlugin plugin;

    /**
     * Resource bundle.
     */
    private ResourceBundle resourceBundle;

    private ImageDescriptorRegistry fImageDescriptorRegistry;

    /**
     * The extension point registry for the
     * <code>org.eclipse.jdt.ui.javaFoldingStructureProvider</code> extension
     * point.
     * 
     * @since 3.0
     */
    private ErlangFoldingStructureProviderRegistry fFoldingStructureProviderRegistry;

    private ProblemMarkerManager fProblemMarkerManager = null;

    private ErlConsoleManager erlConMan;

    /** Key to store custom templates. */
    private static final String CUSTOM_TEMPLATES_KEY = "org.erlide.ui.editor.customtemplates"; //$NON-NLS-1$

    /**
     * The constructor.
     */
    public ErlideUIPlugin() {
        super();
        plugin = this;
        try {
            resourceBundle = ResourceBundle
                    .getBundle("org.erlide.ui.ErlideUIPluginResources");
        } catch (final MissingResourceException x) {
            x.printStackTrace();
            resourceBundle = null;
        }
    }

    /**
     * This method is called upon plug-in activation
     * 
     * @param context
     *            The context
     * @throws Exception
     *             if a problem occurs
     */
    @Override
    public void start(final BundleContext context) throws Exception {
        ErlLogger.debug("Starting UI " + Thread.currentThread());
        super.start(context);

        if (SystemUtils.getInstance().isDeveloper()) {
            BackendManagerPopup.init();
        }

        ErlLogger.debug("Started UI");

        erlConMan = new ErlConsoleManager();
        if (SystemUtils.getInstance().isDeveloper()) {
            try {
                final IBackend ideBackend = BackendCore.getBackendManager()
                        .getIdeBackend();
                if (!ideBackend.hasConsole()) {
                    erlConMan.runtimeAdded(ideBackend);
                }
            } catch (final Exception e) {
                ErlLogger.warn(e);
            }
        }

        startPeriodicDump();
        erlangDebuggerBackendListener = new ErlangDebuggerBackendListener();
        BackendCore.getBackendManager().addBackendListener(
                erlangDebuggerBackendListener);

        startPeriodicCacheCleaner();
    }

    private void startPeriodicCacheCleaner() {
        final Job job = new Job("erlide periodic cache cleaner") {

            @Override
            protected IStatus run(final IProgressMonitor monitor) {
                ErlLogger.info("*** Automatically cleaning caches ***");
                try {
                    try {
                        final List<IEditorReference> editorRefs = getWorkbenchEditorReferences();
                        for (final IEditorReference editorRef : editorRefs) {
                            final IEditorPart editor = editorRef
                                    .getEditor(false);
                            if (editor instanceof ErlangEditor) {
                                final ErlangEditor erlangEditor = (ErlangEditor) editor;
                                ClearCacheAction
                                        .resetCacheForEditor(erlangEditor);
                            }
                        }
                    } catch (final Exception e) {
                        // ignore
                    }
                } finally {
                    schedule(TimeUnit.MILLISECONDS.convert(1, TimeUnit.DAYS));
                }
                return Status.OK_STATUS;
            }

            public List<IEditorReference> getWorkbenchEditorReferences() {
                final IWorkbench workbench = PlatformUI.getWorkbench();
                final IWorkbenchPage[] pages = workbench
                        .getActiveWorkbenchWindow().getPages();
                final List<IEditorReference> editorRefs = Lists.newArrayList();
                for (final IWorkbenchPage page : pages) {
                    for (final IEditorReference ref : page
                            .getEditorReferences()) {
                        editorRefs.add(ref);
                    }
                }
                return editorRefs;
            }

        };
        job.setPriority(Job.SHORT);
        job.setSystem(true);
        job.schedule(getTimeToMidnight());
    }

    private long getTimeToMidnight() {
        final Calendar date = new GregorianCalendar();
        date.set(Calendar.HOUR_OF_DAY, 0);
        date.set(Calendar.MINUTE, 0);
        date.set(Calendar.SECOND, 0);
        date.set(Calendar.MILLISECOND, 0);
        date.add(Calendar.DAY_OF_MONTH, 1);
        final long restOfDayInMilliseconds = date.getTimeInMillis()
                - System.currentTimeMillis();
        return restOfDayInMilliseconds;
    }

    /**
     * This method is called when the plug-in is stopped
     * 
     * @param context
     *            the context
     * @throws Exception
     *             if a problem occurs
     */
    @Override
    public void stop(final BundleContext context) throws Exception {
        erlConMan.dispose();
        super.stop(context);
        BackendCore.getBackendManager().removeBackendListener(
                erlangDebuggerBackendListener);
        if (ErlideImage.isInstalled()) {
            ErlideImage.dispose();
        }
        SWTResourceManager.dispose();
        plugin = null;
    }

    /**
     * Returns the shared instance.
     * 
     * @return The plugin
     */
    public static ErlideUIPlugin getDefault() {
        if (plugin == null) {
            plugin = new ErlideUIPlugin();
        }
        return plugin;
    }

    /**
     * Returns the string from the plugin's resource bundle, or 'key' if not
     * found.
     * 
     * @param key
     *            The resource
     * @return The identified string
     */
    public static String getResourceString(final String key) {
        final ResourceBundle bundle = ErlideUIPlugin.getDefault()
                .getResourceBundle();
        try {

            final String returnString = bundle != null ? bundle.getString(key)
                    : key;
            return returnString;
        } catch (final MissingResourceException e) {
            return key;
        }
    }

    /**
     * Returns the plugin's resource bundle,
     * 
     * @return The requested bundle
     */
    public ResourceBundle getResourceBundle() {
        return resourceBundle;
    }

    /**
     * Returns the standard display to be used. The method first checks, if the
     * thread calling this method has an associated display. If so, this display
     * is returned. Otherwise the method returns the default display.
     * 
     * @return the standard display
     */
    public static Display getStandardDisplay() {
        Display display = Display.getCurrent();
        if (display == null) {
            display = Display.getDefault();
        }

        return display;
    }

    /**
     * Creates an image and places it in the image registry.
     * 
     * @param id
     *            The image id
     * @param baseURL
     *            The descriptor url
     */
    protected void createImageDescriptor(final String id, final URL baseURL) {
        URL url = null;
        try {
            url = new URL(baseURL, ErlideUIConstants.ICON_PATH + id);
        } catch (final MalformedURLException e) {
            // ignore exception
        }

        getImageRegistry().put(id, ImageDescriptor.createFromURL(url));
    }

    /**
     * Returns the image descriptor for the given image PLUGIN_ID. Returns null
     * if there is no such image.
     * 
     * @param id
     *            The image id
     * 
     * @return The image descriptor
     */
    public ImageDescriptor getImageDescriptor(final String id) {
        final ImageDescriptor returnImageDescriptor = getImageRegistry()
                .getDescriptor(id);
        return returnImageDescriptor;
    }

    /**
     * Returns the image for the given image PLUGIN_ID. Returns null if there is
     * no such image.
     * 
     * @param id
     *            The image id
     * 
     * @return The image
     */
    public Image getImage(final String id) {
        final Image returnImage = getImageRegistry().get(id);
        return returnImage;
    }

    /**
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#initializeImageRegistry(org.eclipse.jface.resource.ImageRegistry)
     */
    @Override
    protected void initializeImageRegistry(final ImageRegistry reg) {
        super.initializeImageRegistry(reg);

        final URL baseURL = getBundle().getEntry("/");

        createImageDescriptor(ErlideUIConstants.IMG_CONSOLE, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_NEW_PROJECT_WIZARD, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_PROJECT_LABEL, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_PACKAGE_FOLDER_LABEL,
                baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_PACKAGE_LABEL, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_FILE_LABEL, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_FOLDER_LABEL, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_DISABLED_REFRESH, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_REFRESH, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_DISABLED_IMPORT, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_IMPORT, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_DISABLED_EXPORT, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_EXPORT, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_COLLAPSEALL, baseURL);
        createImageDescriptor(ErlideUIConstants.IMG_PROJECT_CLOSED_LABEL,
                baseURL);

        createImageDescriptor(ErlideUIConstants.IMG_ERLANG_LOGO, baseURL);
    }

    /**
     * @return
     */
    public static IWorkbenchPage getActivePage() {
        final IWorkbenchWindow w = getActiveWorkbenchWindow();
        if (w != null) {
            return w.getActivePage();
        }
        return null;
    }

    public static IWorkbenchWindow getActiveWorkbenchWindow() {
        return getDefault().getWorkbench().getActiveWorkbenchWindow();
    }

    /**
     * Returns the active workbench shell or <code>null</code> if none
     * 
     * @return the active workbench shell or <code>null</code> if none
     */
    public static Shell getActiveWorkbenchShell() {
        final IWorkbenchWindow window = getActiveWorkbenchWindow();
        if (window != null) {
            return window.getShell();
        }
        return null;
    }

    public static void log(final Exception e) {
        log(new Status(IStatus.ERROR, PLUGIN_ID,
                ErlangStatus.INTERNAL_ERROR.getValue(), e.getMessage(), null));
    }

    public static void log(final IStatus status) {
        getDefault().getLog().log(status);
    }

    public static void logErrorMessage(final String message) {
        log(new Status(IStatus.ERROR, PLUGIN_ID,
                ErlangStatus.INTERNAL_ERROR.getValue(), message, null));
    }

    public static void logErrorStatus(final String message, final IStatus status) {
        if (status == null) {
            logErrorMessage(message);
            return;
        }
        final MultiStatus multi = new MultiStatus(PLUGIN_ID,
                ErlangStatus.INTERNAL_ERROR.getValue(), message, null);
        multi.add(status);
        log(multi);
    }

    public static void log(final Throwable e) {
        log(new Status(IStatus.ERROR, PLUGIN_ID,
                ErlangStatus.INTERNAL_ERROR.getValue(),
                "Erlide internal error", e));
    }

    public static ImageDescriptorRegistry getImageDescriptorRegistry() {
        return getDefault().internalGetImageDescriptorRegistry();
    }

    private synchronized ImageDescriptorRegistry internalGetImageDescriptorRegistry() {
        if (fImageDescriptorRegistry == null) {
            fImageDescriptorRegistry = new ImageDescriptorRegistry();
        }
        return fImageDescriptorRegistry;
    }

    /**
     * Returns the registry of the extensions to the
     * <code>org.erlide.ui.erlangFoldingStructureProvider</code> extension
     * point.
     * 
     * @return the registry of contributed
     *         <code>IErlangFoldingStructureProvider</code>
     */
    public synchronized ErlangFoldingStructureProviderRegistry getFoldingStructureProviderRegistry() {
        if (fFoldingStructureProviderRegistry == null) {
            fFoldingStructureProviderRegistry = new ErlangFoldingStructureProviderRegistry();
        }
        return fFoldingStructureProviderRegistry;
    }

    public static void debug(final String message) {
        if (getDefault().isDebugging()) {
            ErlLogger.debug(message);
        }
    }

    public static void createStandardGroups(final IMenuManager menu) {
        if (!menu.isEmpty()) {
            return;
        }
        menu.add(new Separator(IContextMenuConstants.GROUP_OPEN));
        menu.add(new Separator(ITextEditorActionConstants.GROUP_EDIT));
        menu.add(new Separator(IContextMenuConstants.GROUP_SEARCH));
        menu.add(new Separator(IContextMenuConstants.GROUP_ADDITIONS));
        menu.add(new Separator(IContextMenuConstants.GROUP_PROPERTIES));
    }

    /**
     * Returns a section in the Erlang plugin's dialog settings. If the section
     * doesn't exist yet, it is created.
     * 
     * @param name
     *            the name of the section
     * @return the section of the given name
     * @since 3.2
     */
    public IDialogSettings getDialogSettingsSection(final String name) {
        final IDialogSettings dialogSettings = getDialogSettings();
        IDialogSettings section = dialogSettings.getSection(name);
        if (section == null) {
            section = dialogSettings.addNewSection(name);
        }
        return section;
    }

    public ProblemMarkerManager getProblemMarkerManager() {
        if (fProblemMarkerManager == null) {
            fProblemMarkerManager = new ProblemMarkerManager();
        }
        return fProblemMarkerManager;
    }

    public static IEclipsePreferences getPrefsNode() {
        final String qualifier = ErlideUIPlugin.PLUGIN_ID;
        final IScopeContext context = new InstanceScope();
        final IEclipsePreferences eclipsePreferences = context
                .getNode(qualifier);
        return eclipsePreferences;
    }

    private final int DUMP_INTERVAL = Integer.parseInt(System.getProperty(
            "erlide.dump.interval", "300000"));

    private ErlangConsolePage fErlangConsolePage;

    private ContributionContextTypeRegistry fContextTypeRegistry;
    private ContributionTemplateStore fStore;

    private ErlangDebuggerBackendListener erlangDebuggerBackendListener;

    private void startPeriodicDump() {
        final String env = System.getenv("erlide.internal.coredump");
        if (Boolean.parseBoolean(env)) {
            final Job job = new Job("Erlang node info dump") {

                @Override
                protected IStatus run(final IProgressMonitor monitor) {
                    try {
                        final IBackend ideBackend = BackendCore
                                .getBackendManager().getIdeBackend();
                        final String info = BackendHelper
                                .getSystemInfo(ideBackend);
                        final String sep = "\n++++++++++++++++++++++\n";
                        ErlLogger.debug(sep + info + sep);
                    } finally {
                        schedule(DUMP_INTERVAL);
                    }
                    return Status.OK_STATUS;
                }

            };
            job.setPriority(Job.SHORT);
            job.setSystem(true);
            job.schedule(DUMP_INTERVAL);
        }
    }

    public ErlangConsolePage getConsolePage() {
        return fErlangConsolePage;
    }

    public void setConsolePage(final ErlangConsolePage erlangConsolePage) {
        fErlangConsolePage = erlangConsolePage;
    }

    public TemplateStore getTemplateStore() {
        // this is to avoid recursive call when fContextTypeRegistry is null
        getContextTypeRegistry();
        if (fStore == null) {
            fStore = new ErlideContributionTemplateStore(
                    getContextTypeRegistry(), getPreferenceStore(),
                    CUSTOM_TEMPLATES_KEY);
            try {
                fStore.load();
            } catch (final IOException e) {
                getLog().log(
                        new Status(IStatus.ERROR, PLUGIN_ID, IStatus.OK, "", e)); //$NON-NLS-1$
            }
            ErlangSourceContextTypeModule.getDefault().addElementResolvers();
        }
        return fStore;
    }

    public ContextTypeRegistry getContextTypeRegistry() {
        if (fContextTypeRegistry == null) {
            // create an configure the contexts available in the template editor
            fContextTypeRegistry = new ContributionContextTypeRegistry();
            fContextTypeRegistry
                    .addContextType(ErlangContextType.ERLANG_CONTEXT_TYPE_ID);
            fContextTypeRegistry
                    .addContextType(ErlangSourceContextTypeModule.ERLANG_SOURCE_CONTEXT_TYPE_MODULE_ID);
            fContextTypeRegistry
                    .addContextType(ErlangSourceContextTypeModuleElement.ERLANG_SOURCE_CONTEXT_TYPE_MODULE_ELEMENT_ID);
        }
        return fContextTypeRegistry;
    }

    /**
     * Utility method with conventions
     */
    public static void errorDialog(final Shell shell, final String title,
            String message, final Throwable t) {
        IStatus status;
        if (t instanceof CoreException) {
            status = ((CoreException) t).getStatus();
            // if the 'message' resource string and the IStatus' message are the
            // same,
            // don't show both in the dialog
            if (status != null && message.equals(status.getMessage())) {
                message = null;
            }
        } else {
            status = new Status(IStatus.ERROR, PLUGIN_ID,
                    IDebugUIConstants.INTERNAL_ERROR,
                    "Error within Debug UI: ", t); //$NON-NLS-1$
            log(status);
        }
        ErrorDialog.openError(shell, title, message, status);
    }
}
