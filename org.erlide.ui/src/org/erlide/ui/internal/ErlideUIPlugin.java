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

import org.eclipse.core.resources.ResourcesPlugin;
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
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.jface.text.templates.ContextTypeRegistry;
import org.eclipse.jface.text.templates.persistence.TemplateStore;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.editors.text.templates.ContributionContextTypeRegistry;
import org.eclipse.ui.editors.text.templates.ContributionTemplateStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.wb.swt.SWTResourceManager;
import org.erlide.backend.BackendCore;
import org.erlide.core.ConsoleMessageReporter;
import org.erlide.core.ErlangStatus;
import org.erlide.debug.ui.model.ErlangDebuggerBackendListener;
import org.erlide.engine.ErlangEngine;
import org.erlide.ui.ErlideImage;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.UIMessageReporter;
import org.erlide.ui.console.ErlConsoleManager;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.actions.ClearCacheAction;
import org.erlide.ui.internal.folding.ErlangFoldingStructureProviderRegistry;
import org.erlide.ui.perspectives.ErlangPerspective;
import org.erlide.ui.prefs.HighlightStyle;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.templates.ErlangSourceContextTypeModule;
import org.erlide.ui.templates.ErlangSourceContextTypeModuleElement;
import org.erlide.ui.templates.ErlangTemplateContextType;
import org.erlide.ui.templates.ErlideContributionTemplateStore;
import org.erlide.ui.util.BackendManagerPopup;
import org.erlide.ui.util.IContextMenuConstants;
import org.erlide.ui.util.ImageDescriptorRegistry;
import org.erlide.ui.util.NoRuntimeHandler;
import org.erlide.ui.util.ProblemMarkerManager;
import org.erlide.util.ErlLogger;
import org.erlide.util.ErlideEventBus;
import org.erlide.util.HostnameUtils;
import org.erlide.util.SystemConfiguration;
import org.osgi.framework.BundleContext;

import com.google.common.collect.Lists;

/**
 * The main plugin class to be used in the desktop.
 *
 *
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class ErlideUIPlugin extends AbstractUIPlugin {

    public static final String PLUGIN_ID = "org.erlide.ui";

    private static volatile ErlideUIPlugin plugin;

    private ResourceBundle resourceBundle;
    private ImageDescriptorRegistry fImageDescriptorRegistry;
    private ErlangFoldingStructureProviderRegistry fFoldingStructureProviderRegistry;
    private ProblemMarkerManager fProblemMarkerManager = null;
    private ErlConsoleManager erlConsoleManager;

    private static final String CUSTOM_TEMPLATES_KEY = "org.erlide.ui.editor.customtemplates"; //$NON-NLS-1$

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

    @Override
    public void start(final BundleContext context) throws Exception {
        ErlLogger.info("Starting UI " + Thread.currentThread());
        super.start(context);

        final String workspace = ResourcesPlugin.getWorkspace().getRoot().getLocation()
                .toPortableString();
        if (!ErlangEngine.getInstance().isAvailable()) {
            notifyNoRuntimeAndRestart(workspace);
        } else if (HostnameUtils.getErlangHostName(true) == null
                && HostnameUtils.getErlangHostName(false) == null) {
            notifyBadHostname(workspace);
        }

        ErlideEventBus.register(new NoRuntimeHandler());
        ErlideEventBus.register(new ConsoleMessageReporter());
        ErlideEventBus.register(new UIMessageReporter());

        if (SystemConfiguration.getInstance().isDeveloper()) {
            BackendManagerPopup.init();
        }

        loadDefaultEditorColors();

        ErlLogger.info("Started UI");

        erlConsoleManager = new ErlConsoleManager();
        ConsolePlugin.getDefault().getConsoleManager()
                .addConsoleListener(erlConsoleManager);

        erlangDebuggerBackendListener = new ErlangDebuggerBackendListener();
        BackendCore.getBackendManager().addBackendListener(erlangDebuggerBackendListener);

        startPeriodicCacheCleaner();
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        erlConsoleManager.dispose();
        super.stop(context);
        BackendCore.getBackendManager().removeBackendListener(
                erlangDebuggerBackendListener);
        BackendCore.getBackendManager().dispose();
        ErlideImage.dispose();
        SWTResourceManager.dispose();
        plugin = null;
    }

    private void notifyBadHostname(final String workspace) {
        PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
            @Override
            public void run() {
                final Shell activeShell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                final String message = "We are sorry, but your machine's host name is not configured properly "
                        + "and erlide can't work. You need to fix your .hosts file and restart.\n\n";
                final String description = "Java and Erlang can't agree on hostnames. Please check the log in "
                        + workspace
                        + "/erlide.log for details on which names were tried.\n\n"
                        + "Hostnames with dots in them can't be used as short names.\n"
                        + "Hostnames with dashes in them might not always work.\n\n"
                        + "Try to conect two Erlang nodes manually first. Add the working hostname to .hosts.";
                ErrorDialog.openError(activeShell, "Erlide can't work properly", message,
                        new Status(IStatus.ERROR, PLUGIN_ID, description));
            }
        });
    }

    private void notifyNoRuntimeAndRestart(final String workspace) {
        PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
            @Override
            public void run() {
                final Shell activeShell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                final String message = "We are sorry, but the configured Erlang runtime could not be started "
                        + "and erlide can't work. Please check and fix the configuration.";
                final String description = "The log in " + workspace
                        + "/erlide.log may contain more information.";
                ErrorDialog.openError(activeShell, "Erlide can't work properly", message,
                        new Status(IStatus.ERROR, PLUGIN_ID, description));

                final PreferenceDialog pref = PreferencesUtil.createPreferenceDialogOn(
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
                        "org.erlide.ui.preferences.runtimes", null, null);
                if (pref != null) {
                    if (pref.open() == Window.OK) {
                        ErlLogger
                                .info("Restarting workbench after initial runtime configuration...");
                        PlatformUI.getWorkbench().restart();
                    }
                }
            }
        });
    }

    private void loadDefaultEditorColors() {
        final IPreferenceStore rootStore = getPreferenceStore();

        for (final TokenHighlight th : TokenHighlight.values()) {
            final HighlightStyle data = th.getDefaultStyle();
            rootStore.setDefault(th.getColorKey(),
                    StringConverter.asString(data.getColor()));
            rootStore.setDefault(th.getStylesKey(), data.getStyles());
        }
    }

    public ErlConsoleManager getErlConsoleManager() {
        return erlConsoleManager;
    }

    private void startPeriodicCacheCleaner() {
        final Job cacheCleanerJob = new Job("erlide periodic cache cleaner") {

            @Override
            protected IStatus run(final IProgressMonitor monitor) {
                ErlLogger.info("*** Automatically cleaning caches ***");
                try {
                    try {
                        final List<IEditorReference> editorRefs = getWorkbenchEditorReferences();
                        for (final IEditorReference editorRef : editorRefs) {
                            final IEditorPart editor = editorRef.getEditor(false);
                            if (editor instanceof ErlangEditor) {
                                final ErlangEditor erlangEditor = (ErlangEditor) editor;
                                ClearCacheAction.resetCacheForEditor(erlangEditor);
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
                final IWorkbenchPage[] pages = workbench.getActiveWorkbenchWindow()
                        .getPages();
                final List<IEditorReference> editorRefs = Lists.newArrayList();
                for (final IWorkbenchPage page : pages) {
                    for (final IEditorReference ref : page.getEditorReferences()) {
                        editorRefs.add(ref);
                    }
                }
                return editorRefs;
            }

        };
        cacheCleanerJob.setPriority(Job.SHORT);
        cacheCleanerJob.setSystem(true);
        cacheCleanerJob.schedule(getTimeToMidnight());
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
        final ResourceBundle bundle = ErlideUIPlugin.getDefault().getResourceBundle();
        try {

            final String returnString = bundle != null ? bundle.getString(key) : key;
            return returnString;
        } catch (final MissingResourceException e) {
            return key;
        }
    }

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
        createImageDescriptor(ErlideUIConstants.IMG_PACKAGE_FOLDER_LABEL, baseURL);
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
        createImageDescriptor(ErlideUIConstants.IMG_PROJECT_CLOSED_LABEL, baseURL);

        createImageDescriptor(ErlideUIConstants.IMG_ERLANG_LOGO, baseURL);
    }

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

    public static Shell getActiveWorkbenchShell() {
        final IWorkbenchWindow window = getActiveWorkbenchWindow();
        if (window != null) {
            return window.getShell();
        }
        return null;
    }

    public static void log(final Exception e) {
        log(new Status(IStatus.ERROR, PLUGIN_ID, ErlangStatus.INTERNAL_ERROR.getValue(),
                e.getMessage(), null));
    }

    public static void log(final IStatus status) {
        getDefault().getLog().log(status);
    }

    public static void logErrorMessage(final String message) {
        log(new Status(IStatus.ERROR, PLUGIN_ID, ErlangStatus.INTERNAL_ERROR.getValue(),
                message, null));
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
        log(new Status(IStatus.ERROR, PLUGIN_ID, ErlangStatus.INTERNAL_ERROR.getValue(),
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

    public synchronized ErlangFoldingStructureProviderRegistry getFoldingStructureProviderRegistry() {
        if (fFoldingStructureProviderRegistry == null) {
            fFoldingStructureProviderRegistry = new ErlangFoldingStructureProviderRegistry();
        }
        return fFoldingStructureProviderRegistry;
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
        final IScopeContext context = InstanceScope.INSTANCE;
        final IEclipsePreferences eclipsePreferences = context.getNode(qualifier);
        return eclipsePreferences;
    }

    private ContributionContextTypeRegistry fContextTypeRegistry;
    private ContributionTemplateStore fStore;

    private ErlangDebuggerBackendListener erlangDebuggerBackendListener;

    public TemplateStore getTemplateStore() {
        // this is to avoid recursive call when fContextTypeRegistry is null
        getContextTypeRegistry();
        if (fStore == null) {
            fStore = new ErlideContributionTemplateStore(getContextTypeRegistry(),
                    getPreferenceStore(), CUSTOM_TEMPLATES_KEY);
            try {
                fStore.load();
            } catch (final IOException e) {
                getLog().log(new Status(IStatus.ERROR, PLUGIN_ID, IStatus.OK, "", e)); //$NON-NLS-1$
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
                    .addContextType(ErlangTemplateContextType.ERLANG_CONTEXT_TYPE_ID);
            fContextTypeRegistry
                    .addContextType(ErlangSourceContextTypeModule.ERLANG_SOURCE_CONTEXT_TYPE_MODULE_ID);
            fContextTypeRegistry
                    .addContextType(ErlangSourceContextTypeModuleElement.ERLANG_SOURCE_CONTEXT_TYPE_MODULE_ELEMENT_ID);
        }
        return fContextTypeRegistry;
    }

    public static void errorDialog(final Shell shell, final String title,
            final String message0, final Throwable t) {
        IStatus status;
        String message = message0;
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
                    IDebugUIConstants.INTERNAL_ERROR, "Error within Debug UI: ", t); //$NON-NLS-1$
            log(status);
        }
        ErrorDialog.openError(shell, title, message, status);
    }

    public void showErlangPerspective() {
        try {
            getWorkbench().showPerspective(ErlangPerspective.ID,
                    getWorkbench().getActiveWorkbenchWindow());
        } catch (final WorkbenchException we) {
            // ignore
        }
    }
}
