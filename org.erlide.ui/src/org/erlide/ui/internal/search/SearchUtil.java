package org.erlide.ui.internal.search;

import java.text.Collator;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.search.ui.text.Match;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.IWorkingSetSelectionDialog;
import org.eclipse.ui.progress.IProgressService;
import org.erlide.core.ErlangScope;
import org.erlide.core.common.StringUtils;
import org.erlide.core.model.erlang.ErlModelException;
import org.erlide.core.model.erlang.IErlElement;
import org.erlide.core.model.erlang.IErlElement.AcceptFlags;
import org.erlide.core.model.erlang.IErlElement.Kind;
import org.erlide.core.model.erlang.IErlElementVisitor;
import org.erlide.core.model.erlang.IErlExternal;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlModel;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlProject;
import org.erlide.core.model.erlang.IParent;
import org.erlide.core.model.erlang.ModuleKind;
import org.erlide.core.model.erlang.util.ErlideUtil;
import org.erlide.core.model.erlang.util.ModelUtils;
import org.erlide.core.services.search.ErlSearchScope;
import org.erlide.core.services.search.ErlangSearchPattern;
import org.erlide.core.services.search.ErlangSearchPattern.LimitTo;
import org.erlide.core.services.search.ErlangSearchPattern.SearchFor;
import org.erlide.core.services.search.FunctionPattern;
import org.erlide.core.services.search.IncludePattern;
import org.erlide.core.services.search.MacroPattern;
import org.erlide.core.services.search.ModuleLineFunctionArityRef;
import org.erlide.core.services.search.OpenResult;
import org.erlide.core.services.search.RecordFieldPattern;
import org.erlide.core.services.search.RecordPattern;
import org.erlide.core.services.search.TypeRefPattern;
import org.erlide.core.services.search.VariablePattern;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;
import org.osgi.framework.Bundle;

import com.google.common.collect.Sets;

public class SearchUtil {

    static final int SEARCH_IN_SOURCES = 1;
    static final int SEARCH_IN_EXTERNALS = 2;
    static final int SEARCH_IN_OTP_LIBRARIES = 4;

    private static final int ARI_TYPESPEC = -2;
    private static final int ARI_ATTRIBUTE = -3;
    private static final int ARI_RECORD_DEF = -4;
    private static final int ARI_MACRO_DEF = -5;
    private static final int ARI_INCLUDE = -6;
    private static final int ARI_RECORD_FIELD_DEF = -7;

    public static final class WorkingSetComparator implements
            Comparator<IWorkingSet> {
        private static Collator collator = Collator.getInstance();

        public int compare(final IWorkingSet o1, final IWorkingSet o2) {
            return collator.compare(o1.getLabel(), o2.getLabel());
        }
    }

    static public ErlSearchScope getProjectsScope(
            final Collection<IProject> projects) {
        final ErlSearchScope result = new ErlSearchScope();
        for (final IProject project : projects) {
            addProjectToScope(project, result);
        }
        return result;
    }

    private static void addProjectToScope(final IProject project,
            final ErlSearchScope result) {
        if (project == null) {
            return;
        }
        final IErlProject erlProject = ErlangScope.getModel().getErlangProject(
                project);
        final Collection<IPath> sourcePaths = erlProject.getSourceDirs();
        for (final IPath path : sourcePaths) {
            final IFolder folder = project.getFolder(path);
            addFolderToScope(folder, result);
        }
    }

    private static void addFolderToScope(final IFolder folder,
            final ErlSearchScope result) {
        if (folder != null) {
            try {
                for (final IResource r : folder.members()) {
                    if (r instanceof IFile) {
                        final IFile f = (IFile) r;
                        addFileToScope(f, result);
                    }
                }
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
    }

    private static void addFileToScope(final IFile file,
            final ErlSearchScope result) {
        if (ModuleKind.hasModuleExtension(file.getName())) {
            final IErlModule module = ErlangScope.getModel().findModule(file);
            result.addModule(module);
        }
    }

    static public ErlSearchScope getWorkspaceScope() {
        final ErlSearchScope result = new ErlSearchScope();
        try {
            final Collection<IErlProject> erlangProjects = ErlangScope
                    .getModel().getErlangProjects();
            for (final IErlProject i : erlangProjects) {
                final Collection<IErlModule> modules = i
                        .getModulesAndIncludes();
                for (final IErlModule j : modules) {
                    result.addModule(j);
                }
                // addProjectEbin(i, result);
            }
        } catch (final ErlModelException e) {
            ErlLogger.error(e); // TODO report this
        }
        return result;
    }

    public static ErlSearchScope getWorkspaceExternalScope(
            final boolean addExternals, final boolean addOtp) {
        try {
            final Collection<IErlProject> erlangProjects = ErlangScope
                    .getModel().getErlangProjects();
            final ErlSearchScope result = new ErlSearchScope();
            final Set<String> externalModulePaths = new HashSet<String>();
            for (final IErlProject project : erlangProjects) {
                addExternalModules(project, result, externalModulePaths,
                        addExternals, addOtp);
            }
            return result;
        } catch (final ErlModelException e) {
            ErlLogger.error(e); // TODO report this
        }
        return null;
    }

    private static void addExternalModules(final IParent element,
            final ErlSearchScope result, final Set<String> externalModulePaths,
            final boolean addExternals, final boolean addOtp)
            throws ErlModelException {
        final Collection<IErlElement> externals = element
                .getChildrenOfKind(Kind.EXTERNAL);
        for (final IErlElement external : externals) {
            external.accept(new IErlElementVisitor() {

                public boolean visit(final IErlElement theElement)
                        throws ErlModelException {
                    if (theElement instanceof IErlExternal) {
                        final IErlExternal theExternal = (IErlExternal) theElement;
                        if (theExternal.isOTP()) {
                            if (!addOtp) {
                                return false;
                            }
                        } else {
                            if (!addExternals) {
                                return false;
                            }
                        }
                        theExternal.open(null);
                    }
                    if (theElement instanceof IErlModule) {
                        final IErlModule module = (IErlModule) theElement;
                        if (externalModulePaths.add(module.getFilePath())) {
                            result.addModule(module);
                        }
                    }
                    return true;
                }
            }, EnumSet.noneOf(AcceptFlags.class), Kind.MODULE);
        }
    }

    public static Collection<IProject> getProjects(final String[] projectNames) {
        final Collection<IProject> result = Sets
                .newHashSetWithExpectedSize(projectNames.length);
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        for (final String i : projectNames) {
            final IProject project = root.getProject(i);
            result.add(project);
        }
        return result;
    }

    public static ErlSearchScope getProjectsExternalScope(
            final String[] projectNames, final boolean addExternals,
            final boolean addOtp) {
        final ErlSearchScope result = new ErlSearchScope();
        final Set<String> externalModulePaths = new HashSet<String>();
        try {
            final IWorkspaceRoot root = ResourcesPlugin.getWorkspace()
                    .getRoot();
            final IErlModel model = ErlangScope.getModel();
            for (final String i : projectNames) {
                final IProject project = root.getProject(i);
                if (ErlideUtil.hasErlangNature(project)) {
                    final IErlProject erlProject = model
                            .getErlangProject(project);
                    addExternalModules(erlProject, result, externalModulePaths,
                            addExternals, addOtp);
                }
            }
        } catch (final ErlModelException e) {
            ErlLogger.error(e); // TODO report this
        }
        return result;
    }

    public static ErlSearchScope getSelectionScope(final ISelection selection) {
        final ErlSearchScope result = new ErlSearchScope();
        if (selection instanceof IStructuredSelection) {
            final IStructuredSelection ss = (IStructuredSelection) selection;
            for (final Object i : ss.toArray()) {
                if (i instanceof IResource) {
                    final IResource r = (IResource) i;
                    addResourceToScope(result, r);
                }
            }
        }
        return result;
    }

    public static ErlSearchScope getSelectionExternalScope(
            final ISelection selection, final boolean addExternals,
            final boolean addOtp) {
        final ErlSearchScope result = new ErlSearchScope();
        final Set<String> externalModulePaths = new HashSet<String>();
        try {
            if (selection instanceof IStructuredSelection) {
                final IStructuredSelection ss = (IStructuredSelection) selection;
                for (final Object i : ss.toArray()) {
                    if (i instanceof IParent) {
                        final IParent parent = (IParent) i;
                        addExternalModules(parent, result, externalModulePaths,
                                addExternals, addOtp);
                    }
                }
            }
        } catch (final ErlModelException e) {
            ErlLogger.error(e); // TODO report this
        }
        return result;
    }

    public static void runQueryInBackground(final Object query) {
        NewSearchUI.runQueryInBackground((ISearchQuery) query);
    }

    public static IStatus runQueryInForeground(
            final IProgressService progressService, final Object query) {
        return NewSearchUI.runQueryInForeground(progressService,
                (ISearchQuery) query);
    }

    public static boolean isLineDelimiterChar(final char ch) {
        return ch == '\n' || ch == '\r';
    }

    public static Match createMatch(final ModuleLineFunctionArityRef ref,
            final Map<String, IErlModule> pathToModuleMap) {
        final ErlangSearchElement ese = createSearchElementFromRef(ref,
                pathToModuleMap);
        return new Match(ese, ref.getOffset(), ref.getLength());
    }

    public static ErlangSearchElement createSearchElementFromRef(
            final ModuleLineFunctionArityRef ref,
            final Map<String, IErlModule> pathToModuleMap) {
        final IErlModule module = pathToModuleMap.get(ref.getModulePath());
        return createSearchElement(ref, module);
    }

    public static ErlangSearchElement createSearchElement(
            final ModuleLineFunctionArityRef ref, final IErlModule module) {
        return new ErlangSearchElement(module, ref.getModulePath(),
                ref.getName(), ref.getArity(), ref.getClauseHead(),
                ref.isSubClause(), refToKind(ref));
    }

    public static Kind refToKind(final ModuleLineFunctionArityRef ref) {
        switch (ref.getArity()) {
        case ARI_TYPESPEC:
            return Kind.TYPESPEC;
        case ARI_ATTRIBUTE:
            return Kind.ATTRIBUTE; // Kind.MODULE; ?
        case ARI_RECORD_DEF:
            return Kind.RECORD_DEF;
        case ARI_MACRO_DEF:
            return Kind.MACRO_DEF;
        case ARI_INCLUDE:
            return Kind.ATTRIBUTE;
            // include actually, attributes are not saved (yet)
        case ARI_RECORD_FIELD_DEF:
            return Kind.RECORD_FIELD;
        default:
            if (ref.isSubClause()) {
                return Kind.CLAUSE;
            }
            return Kind.FUNCTION;
        }
    }

    public static ErlangSearchPattern getSearchPatternFromOpenResultAndLimitTo(
            final IErlModule module, final int offset, final OpenResult res,
            final LimitTo limitTo, final boolean matchAnyFunctionDefinition) {
        if (res == null) {
            return null;
        }
        String name = res.getName();
        final String unquoted = name != null ? StringUtils.unquote(name) : null;
        if (res.isExternalCall()) {
            if (module != null && offset != -1) {
                try {
                    final IErlElement e = module.getElementAt(offset);
                    if (e != null
                            && (e.getKind() == Kind.TYPESPEC || e.getKind() == Kind.RECORD_DEF)) {
                        return new TypeRefPattern(name, res.getFun(), limitTo);
                    }
                } catch (final ErlModelException e1) {
                    ErlLogger.warn(e1);
                }
            }
            String oldName;
            name = unquoted;
            do {
                oldName = name;
                name = ModelUtils.resolveMacroValue(name, module);
            } while (!name.equals(oldName));
            return new FunctionPattern(name, res.getFun(), res.getArity(),
                    limitTo, matchAnyFunctionDefinition);
        } else if (res.isLocalCall()) {
            if (module != null) {
                name = module.getModuleName();
                if (offset != -1) {
                    try {
                        final IErlElement e = module.getElementAt(offset);
                        if (e != null
                                && (e.getKind() == Kind.TYPESPEC || e.getKind() == Kind.RECORD_DEF)) {
                            return new TypeRefPattern(name, res.getFun(),
                                    limitTo);
                        }
                    } catch (final ErlModelException e1) {
                        ErlLogger.warn(e1);
                    }
                }
            } else {
                name = null;
            }
            return new FunctionPattern(name, res.getFun(), res.getArity(),
                    limitTo, matchAnyFunctionDefinition);
        } else if (res.isMacro()) {
            return new MacroPattern(unquoted, limitTo);
        } else if (res.isRecord()) {
            return new RecordPattern(unquoted, limitTo);
        } else if (res.isInclude()) {
            return new IncludePattern(name, limitTo);
        } else if (res.isVariable()) {
            if (module != null) {
                name = module.getModuleName();
                if (offset != -1) {
                    try {
                        final IErlElement e = module.getElementAt(offset);
                        if (e instanceof IErlFunctionClause) {
                            final IErlFunctionClause c = (IErlFunctionClause) e;
                            return new VariablePattern(c.getFunctionName(),
                                    c.getArity(), c.getHead(), res.getName(),
                                    limitTo);
                        }
                    } catch (final ErlModelException e1) {
                    }
                }
            }
        } else if (res.isField()) {
            return new RecordFieldPattern(res.getFun(), unquoted, limitTo);
        }
        return null;
    }

    public static ErlangSearchPattern getSearchPattern(final IErlModule module,
            final SearchFor searchFor, final String pattern,
            final LimitTo limitTo) {
        String moduleName = "", name = pattern;
        int arity = 0;
        int p = pattern.indexOf(':');
        if (p != -1) {
            moduleName = pattern.substring(0, p);
            name = pattern.substring(p + 1);
        }
        p = name.indexOf('/');
        if (p != -1) {
            arity = Integer.valueOf(name.substring(p + 1));
            name = name.substring(0, p);
        }
        return ErlangSearchPattern.getSearchPattern(searchFor, moduleName,
                name, arity, limitTo);
    }

    public static void runQuery(final ErlangSearchPattern ref,
            final ErlSearchScope scope, final ErlSearchScope externalScope,
            final String scopeDescription, final Shell shell) {
        final ErlSearchQuery query = new ErlSearchQuery(ref, scope,
                externalScope, scopeDescription);
        if (query.canRunInBackground()) {
            /*
             * This indirection with Object as parameter is needed to prevent
             * the loading of the Search plug-in: the VM verifies the method
             * call and hence loads the types used in the method signature,
             * eventually triggering the loading of a plug-in (in this case
             * ISearchQuery results in Search plug-in being loaded).
             */
            NewSearchUI.runQueryInBackground(query);
        } else {
            final IProgressService progressService = PlatformUI.getWorkbench()
                    .getProgressService();
            /*
             * This indirection with Object as parameter is needed to prevent
             * the loading of the Search plug-in: the VM verifies the method
             * call and hence loads the types used in the method signature,
             * eventually triggering the loading of a plug-in (in this case it
             * would be ISearchQuery).
             */
            final IStatus status = NewSearchUI.runQueryInForeground(
                    progressService, query);
            if (status.matches(IStatus.ERROR | IStatus.INFO | IStatus.WARNING)) {
                ErrorDialog.openError(shell,
                        "SearchMessages.Search_Error_search_title",
                        "SearchMessages.Search_Error_search_message", status);
            }
        }
    }

    public static String getWorkingSetsScopeDescription(
            final IWorkingSet[] workingSets) {
        final String wssS = "working sets ";
        final String wsS = "working set ";
        if (workingSets.length == 0) {
            return "";
        }
        final String s = workingSets.length == 1 ? wsS : wssS;
        return workingSetLabels(workingSets, s, "'");
    }

    private static String workingSetLabels(final IWorkingSet[] workingSets,
            final String s, final String surround) {
        final StringBuilder sb = new StringBuilder(s);
        int i = 0;
        for (final IWorkingSet ws : workingSets) {
            sb.append(surround).append(ws.getLabel()).append(surround)
                    .append(", ");
            i++;
            if (i == 2) {
                break;
            }
        }
        if (workingSets.length > 2) {
            return sb.append("... ").toString();
        }
        return sb.substring(0, sb.length() - 2);
    }

    public static ErlSearchScope getWorkingSetsScope(
            final IWorkingSet[] workingSets) {
        final ErlSearchScope result = new ErlSearchScope();
        if (workingSets == null) {
            return result;
        }
        for (final IWorkingSet ws : workingSets) {
            final IAdaptable[] elements = ws.getElements();
            for (final IAdaptable a : elements) {
                final IResource r = (IResource) a.getAdapter(IResource.class);
                addResourceToScope(result, r);
            }
        }
        return result;
    }

    public static ErlSearchScope getWorkingSetsExternalScope(
            final IWorkingSet[] workingSets, final boolean addExternals,
            final boolean addOTP) {
        final ErlSearchScope result = new ErlSearchScope();
        final Set<String> externalModulePaths = new HashSet<String>();
        try {
            if (workingSets == null) {
                return result;
            }
            for (final IWorkingSet ws : workingSets) {
                final IAdaptable[] elements = ws.getElements();
                for (final IAdaptable a : elements) {
                    IParent parent = null;
                    Object o = a.getAdapter(IErlElement.class);
                    if (o instanceof IParent) {
                        parent = (IParent) o;
                    } else {
                        o = a.getAdapter(IResource.class);
                        if (o != null) {
                            final IResource resource = (IResource) o;
                            final IErlElement element = ErlangScope.getModel()
                                    .findElement(resource);
                            if (element instanceof IParent) {
                                parent = (IParent) element;
                            }
                        }
                    }
                    if (parent != null) {
                        addExternalModules(parent, result, externalModulePaths,
                                addExternals, addOTP);
                    }
                }
            }
        } catch (final ErlModelException e) {
            ErlLogger.error(e); // TODO report this
        }
        return result;
    }

    private static void addResourceToScope(final ErlSearchScope result,
            final IResource r) {
        if (r instanceof IProject) {
            final IProject project = (IProject) r;
            addProjectToScope(project, result);
        } else if (r instanceof IFile) {
            final IFile file = (IFile) r;
            addFileToScope(file, result);
        } else if (r instanceof IFolder) {
            final IFolder folder = (IFolder) r;
            addFolderToScope(folder, result);
        }
    }

    public static String getProjectScopeDescription(
            final Collection<IProject> projects) {
        if (projects == null || projects.isEmpty()) {
            return "";
        } else {
            final StringBuilder sb = new StringBuilder(
                    projects.size() == 1 ? "project" : "projects");
            sb.append(' ');
            int i = 0;
            for (final IProject p : projects) {
                sb.append('\'').append(p.getName()).append("', ");
                i++;
                if (i == 2) {
                    break;
                }
            }
            if (projects.size() > 2) {
                return sb.append("... ").toString();
            }
            return sb.substring(0, sb.length() - 2);
        }
    }

    public static String getSelectionScopeDescription(final ISelection selection) {
        if (selection instanceof IStructuredSelection) {
            final StringBuilder sb = new StringBuilder();
            final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
            int i = 0;
            for (final Object o : structuredSelection.toList()) {
                if (o instanceof IResource) {
                    final IResource resource = (IResource) o;
                    sb.append('\'').append(resource.getName()).append("', ");
                    i++;
                    if (i == 2) {
                        break;
                    }
                }
            }
            if (structuredSelection.size() > 2) {
                return sb.append("...").toString();
            }
            return sb.substring(0, sb.length() - 2);
        }
        return "";
    }

    public static String getWorkspaceScopeDescription() {
        return "workspace";
    }

    public static String toString(final IWorkingSet[] workingSets) {
        Arrays.sort(workingSets, new WorkingSetComparator());
        return workingSetLabels(workingSets, "", "");
    }

    // LRU working sets
    public static final int LRU_WORKINGSET_LIST_SIZE = 3;
    private static LRUWorkingSetsList fgLRUWorkingSets;

    // Settings store
    private static final String DIALOG_SETTINGS_KEY = "JavaElementSearchActions";
    private static final String STORE_LRU_WORKING_SET_NAMES = "lastUsedWorkingSetNames";

    // private static final String BIN_PRIM_CONST_WARN_DIALOG_ID =
    // "BinaryPrimitiveConstantWarningDialog";

    public static boolean isSearchPlugInActivated() {
        return Platform.getBundle("org.eclipse.search").getState() == Bundle.ACTIVE;
    }

    private static IDialogSettings getDialogStoreSection() {
        final IDialogSettings dialogSettings = ErlideUIPlugin.getDefault()
                .getDialogSettings();
        IDialogSettings settingsStore = dialogSettings
                .getSection(DIALOG_SETTINGS_KEY);
        if (settingsStore == null) {
            settingsStore = dialogSettings.addNewSection(DIALOG_SETTINGS_KEY);
        }
        return settingsStore;
    }

    public static LRUWorkingSetsList getLRUWorkingSets() {
        if (fgLRUWorkingSets == null) {
            restoreState();
        }
        return fgLRUWorkingSets;
    }

    private static void restoreState() {
        fgLRUWorkingSets = new LRUWorkingSetsList(LRU_WORKINGSET_LIST_SIZE);
        final IDialogSettings settingsStore = getDialogStoreSection();

        for (int i = LRU_WORKINGSET_LIST_SIZE - 1; i >= 0; i--) {
            final String[] lruWorkingSetNames = settingsStore
                    .getArray(STORE_LRU_WORKING_SET_NAMES + i);
            if (lruWorkingSetNames != null) {
                final Set<IWorkingSet> workingSets = new HashSet<IWorkingSet>(2);
                for (int j = 0; j < lruWorkingSetNames.length; j++) {
                    final IWorkingSet workingSet = PlatformUI.getWorkbench()
                            .getWorkingSetManager()
                            .getWorkingSet(lruWorkingSetNames[j]);
                    if (workingSet != null) {
                        workingSets.add(workingSet);
                    }
                }
                if (!workingSets.isEmpty()) {
                    fgLRUWorkingSets.add(workingSets
                            .toArray(new IWorkingSet[workingSets.size()]));
                }
            }
        }
    }

    public static IWorkingSet[] queryWorkingSets() throws InterruptedException {
        final IWorkbenchWindow activeWorkbenchWindow = PlatformUI
                .getWorkbench().getActiveWorkbenchWindow();
        if (activeWorkbenchWindow == null) {
            return null;
        }
        final Shell shell = activeWorkbenchWindow.getShell();
        if (shell == null) {
            return null;
        }
        final IWorkingSetSelectionDialog dialog = PlatformUI.getWorkbench()
                .getWorkingSetManager()
                .createWorkingSetSelectionDialog(shell, true);
        if (dialog.open() != Window.OK) {
            throw new InterruptedException();
        }

        final IWorkingSet[] workingSets = dialog.getSelection();
        if (workingSets.length > 0) {
            return workingSets;
        }
        return null; // 'no working set' selected
    }

    public static void updateLRUWorkingSets(final IWorkingSet[] workingSets) {
        if (workingSets == null || workingSets.length < 1) {
            return;
        }
        getLRUWorkingSets().add(workingSets);
        saveState(getDialogStoreSection());
    }

    private static void saveState(final IDialogSettings settingsStore) {
        int i = 0;
        for (final IWorkingSet[] workingSets : fgLRUWorkingSets.get()) {
            final String[] names = new String[workingSets.length];
            for (int j = 0; j < workingSets.length; j++) {
                names[j] = workingSets[j].getName();
            }
            settingsStore.put(STORE_LRU_WORKING_SET_NAMES + i, names);
            i++;
        }
    }

}
