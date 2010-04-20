package org.erlide.ui.internal.search;

import java.text.Collator;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
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
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlAttribute;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlMacroDef;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlRecordDef;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.core.search.ErlangSearchPattern;
import org.erlide.core.search.ModuleLineFunctionArityRef;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.util.ErlModelUtils;
import org.osgi.framework.Bundle;

import erlang.ErlideOpen;
import erlang.OpenResult;

public class SearchUtil {

	private static final int ARI_TYPESPEC = -2;
	private static final int ARI_ATTRIBUTE = -3;
	private static final int ARI_RECORD_DEF = -4;
	private static final int ARI_MACRO_DEF = -5;
	private static final int ARI_INCLUDE = -6;

	public static final class WorkingSetComparator implements
			Comparator<IWorkingSet> {
		private static Collator collator = Collator.getInstance();

		public int compare(final IWorkingSet o1, final IWorkingSet o2) {
			return collator.compare(o1.getLabel(), o2.getLabel());
		}
	}

	static public Collection<IResource> getProjectScope(final IProject project) {
		final Set<IResource> result = new HashSet<IResource>();
		addProjectToScope(project, result);
		return result;
	}

	private static void addProjectToScope(final IProject project,
			final Collection<IResource> result) {
		if (project == null) {
			return;
		}
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		final List<String> sourcePaths = prefs.getSourceDirs();
		for (String path : sourcePaths) {
			IFolder folder = project.getFolder(new Path(path));
			addFolderToScope(folder, result);
		}
	}

	private static void addFolderToScope(final IFolder folder,
			final Collection<IResource> result) {
		if (folder != null) {
			try {
				for (IResource r : folder.members()) {
					if (r instanceof IFile) {
						IFile f = (IFile) r;
						addFileToScope(f, result);
					}
				}
			} catch (CoreException e) {
				e.printStackTrace();
			}
		}
	}

	private static void addFileToScope(final IFile file,
			final Collection<IResource> result) {
		if (ErlideUtil.hasModuleExtension(file.getName())) {
			result.add(file);
		}
	}

	static public Collection<IResource> getWorkspaceScope() {
		try {
			final Collection<IErlProject> erlangProjects = ErlangCore
					.getModel().getErlangProjects();
			final Set<IResource> result = new HashSet<IResource>();
			for (final IErlProject i : erlangProjects) {
				final List<IErlModule> modules = i.getModulesAndHeaders();
				for (final IErlModule j : modules) {
					result.add(j.getResource());
				}
				// addProjectEbin(i, result);
			}
			return result;
		} catch (final ErlModelException e) {
			ErlLogger.error(e); // TODO report this
		}
		return null;
	}

	public static Collection<IResource> getProjectsScope(
			final String[] projectNames) {
		final Set<IResource> result = new HashSet<IResource>();
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		for (final String i : projectNames) {
			IProject project = root.getProject(i);
			addProjectToScope(project, result);
		}
		return result;
	}

	public static Collection<IResource> getSelectionScope(
			final ISelection selection) {
		Set<IResource> result = new HashSet<IResource>();
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection ss = (IStructuredSelection) selection;
			for (Object i : ss.toArray()) {
				if (i instanceof IResource) {
					IResource r = (IResource) i;
					addResourceToScope(result, r);
				}
			}
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

	public static ErlangSearchPattern getSearchPatternFromErlElementAndLimitTo(
			final IErlElement element, final int limitTo) {
		if (element instanceof IErlFunction) {
			final IErlFunction function = (IErlFunction) element;
			String withoutExtension = ErlideUtil.withoutExtension(function
					.getModule().getName());
			return new ErlangSearchPattern(
					ErlangSearchPattern.SEARCHFOR_FUNCTION, withoutExtension,
					function.getFunctionName(), function.getArity(), limitTo);
		} else if (element instanceof IErlMacroDef) {
			IErlMacroDef m = (IErlMacroDef) element;
			String unquote = ErlideUtil.unquote(m.getDefinedName());
			return new ErlangSearchPattern(ErlangSearchPattern.SEARCHFOR_MACRO,
					null, unquote, -1, limitTo);
		} else if (element instanceof IErlRecordDef) {
			IErlRecordDef r = (IErlRecordDef) element;
			String unquote = ErlideUtil.unquote(r.getDefinedName());
			return new ErlangSearchPattern(
					ErlangSearchPattern.SEARCHFOR_RECORD, null, unquote, -1,
					limitTo);
		} else if (element instanceof IErlFunctionClause) {
			final IErlFunctionClause clause = (IErlFunctionClause) element;
			return getSearchPatternFromErlElementAndLimitTo(clause.getParent(),
					limitTo);
		} else if (element instanceof IErlAttribute) {
			IErlAttribute a = (IErlAttribute) element;
			if (a.getName().startsWith("include")) {
				String s = Util.stringValue(a.getValue());
				return new ErlangSearchPattern(
						ErlangSearchPattern.SEARCHFOR_INCLUDE, null, s, -1,
						limitTo);
			}
		}
		return null;
	}

	public static boolean isLineDelimiterChar(final char ch) {
		return ch == '\n' || ch == '\r';
	}

	public static Match createMatch(final ModuleLineFunctionArityRef ref) {
		final ErlangSearchElement ese = new ErlangSearchElement(ref
				.getModuleName(), ref.getName(), ref.getArity(), ref
				.getClauseHead(), ref.isSubClause(), refToKind(ref));
		return new Match(ese, ref.getOffset(), ref.getLength());
	}

	private static Kind refToKind(final ModuleLineFunctionArityRef ref) {
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
			return Kind.ATTRIBUTE; // include actually, attributes are not saved
			// (yet)
		default:
			if (ref.isSubClause()) {
				return Kind.CLAUSE;
			}
			return Kind.FUNCTION;
		}
	}

	public static ErlangSearchPattern getSearchPatternFromOpenResultAndLimitTo(
			final IErlModule module, final int offset, final OpenResult res,
			final int limitTo) {
		ErlangSearchPattern ref = null;
		String name = res.getName();
		String unquoted = name != null ? ErlideUtil.unquote(name) : null;
		if (res.isExternalCall()) {
			if (module != null && offset != -1) {
				try {
					IErlElement e = module.getElementAt(offset);
					if (e != null
							&& (e.getKind() == Kind.TYPESPEC || e.getKind() == Kind.RECORD_DEF)) {
						return new ErlangSearchPattern(
								ErlangSearchPattern.SEARCHFOR_TYPE, name, res
										.getFun(), 1, limitTo);
					}
				} catch (ErlModelException e1) {
					ErlLogger.warn(e1);
				}
			}
			return new ErlangSearchPattern(
					ErlangSearchPattern.SEARCHFOR_FUNCTION, name, res.getFun(),
					res.getArity(), limitTo);
		} else if (res.isLocalCall()) {
			if (module != null) {
				name = module.getModuleName();
				if (offset != -1) {
					try {
						IErlElement e = module.getElementAt(offset);
						if (e != null
								&& (e.getKind() == Kind.TYPESPEC || e.getKind() == Kind.RECORD_DEF)) {
							return new ErlangSearchPattern(
									ErlangSearchPattern.SEARCHFOR_TYPE, name,
									res.getFun(), 1, limitTo);
						}
					} catch (ErlModelException e1) {
						ErlLogger.warn(e1);
					}
				}
			} else {
				name = null;
			}
			return new ErlangSearchPattern(
					ErlangSearchPattern.SEARCHFOR_FUNCTION, name, res.getFun(),
					res.getArity(), limitTo);
		} else if (res.isMacro()) {
			return new ErlangSearchPattern(ErlangSearchPattern.SEARCHFOR_MACRO,
					null, unquoted, 0, limitTo);
		} else if (res.isRecord()) {
			return new ErlangSearchPattern(
					ErlangSearchPattern.SEARCHFOR_RECORD, null, unquoted, 0,
					limitTo);
		} else if (res.isInclude()) {
			return new ErlangSearchPattern(
					ErlangSearchPattern.SEARCHFOR_INCLUDE, null, name, 0,
					limitTo);
		}
		return ref;
	}

	public static ErlangSearchPattern getSearchPattern(final IErlModule module,
			final int searchFor, final String pattern, final int limitTo) {
		if (searchFor == ErlangSearchPage.SEARCHFOR_ANYTHING) {
			final Backend b = ErlangCore.getBackendManager().getIdeBackend();
			try {
				OpenResult res = ErlideOpen.getOpenInfo(b, pattern,
						ErlModelUtils.getImportsAsList(module), "", ErlangCore
								.getModel().getPathVars());
				ErlLogger.debug("find " + res);
				return SearchUtil.getSearchPatternFromOpenResultAndLimitTo(
						module, -1, res, limitTo);
			} catch (BackendException e) {
				e.printStackTrace();
			}
		} else {
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
			return new ErlangSearchPattern(searchFor, moduleName, name, arity,
					limitTo);
		}
		return null;
	}

	public static void runQuery(final ErlangSearchPattern ref,
			final Collection<IResource> scope, final String scopeDescription,
			final Shell shell) {
		final ErlSearchQuery query = new ErlSearchQuery(ref, scope,
				scopeDescription);
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
		String wssS = "working sets ";
		String wsS = "working set ";
		if (workingSets.length == 0) {
			return "";
		}
		String s = workingSets.length == 1 ? wsS : wssS;
		return workingSetLabels(workingSets, s, "'");
	}

	private static String workingSetLabels(final IWorkingSet[] workingSets,
			final String s, final String surround) {
		StringBuilder sb = new StringBuilder(s);
		for (IWorkingSet ws : workingSets) {
			sb.append(surround).append(ws.getLabel()).append(surround).append(
					", ");
		}
		return sb.substring(0, sb.length() - 2);
	}

	public static Collection<IResource> getWorkingSetsScope(
			final IWorkingSet[] workingSets) {
		Set<IResource> result = new HashSet<IResource>();
		if (workingSets == null) {
			return result;
		}
		for (IWorkingSet ws : workingSets) {
			IAdaptable[] elements = ws.getElements();
			for (IAdaptable a : elements) {
				IResource r = (IResource) a.getAdapter(IResource.class);
				addResourceToScope(result, r);
			}
		}
		return result;
	}

	private static void addResourceToScope(final Set<IResource> result,
			final IResource r) {
		if (r instanceof IProject) {
			IProject project = (IProject) r;
			addProjectToScope(project, result);
		} else if (r instanceof IFile) {
			IFile file = (IFile) r;
			addFileToScope(file, result);
		} else if (r instanceof IFolder) {
			IFolder folder = (IFolder) r;
			addFolderToScope(folder, result);
		}
	}

	public static String getProjectScopeDescription(
			final Collection<IResource> projectScope) {
		if (projectScope.size() == 0) {
			return "";
		} else {
			StringBuilder sb = new StringBuilder(
					projectScope.size() == 1 ? "project" : "projects");
			sb.append(' ');
			for (IResource p : projectScope) {
				sb.append('\'').append(p.getName()).append("', ");
			}
			return sb.substring(0, sb.length() - 2);
		}
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
		IDialogSettings dialogSettings = ErlideUIPlugin.getDefault()
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
							.getWorkingSetManager().getWorkingSet(
									lruWorkingSetNames[j]);
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
		IWorkbenchWindow activeWorkbenchWindow = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null) {
			return null;
		}
		Shell shell = activeWorkbenchWindow.getShell();
		if (shell == null) {
			return null;
		}
		IWorkingSetSelectionDialog dialog = PlatformUI.getWorkbench()
				.getWorkingSetManager().createWorkingSetSelectionDialog(shell,
						true);
		if (dialog.open() != Window.OK) {
			throw new InterruptedException();
		}

		IWorkingSet[] workingSets = dialog.getSelection();
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
		for (IWorkingSet[] workingSets : fgLRUWorkingSets.get()) {
			String[] names = new String[workingSets.length];
			for (int j = 0; j < workingSets.length; j++) {
				names[j] = workingSets[j].getName();
			}
			settingsStore.put(STORE_LRU_WORKING_SET_NAMES + i, names);
			i++;
		}
	}

}
