package org.erlide.ui.search;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.ui.progress.IProgressService;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.search.ErlangExternalFunctionCallRef;

import erlang.OpenResult;

public class SearchUtil {

	// static public String[] getWorkingSetsScope(final IWorkingSet[]
	// workingSets) {
	// return null;
	// }

	static public String[] getProjectScope(final IProject project) {
		return null;
	}

	static public String[] getWorkspaceScope() {
		return null;
	}

	public static String[] getProjectsScope(final String[] projectNames) {
		// TODO Auto-generated method stub
		return null;
	}

	public static String[] getSelectionScope(final ISelection selection) {
		// TODO Auto-generated method stub
		return null;
	}

	public static void runQueryInBackground(final Object query) {
		NewSearchUI.runQueryInBackground((ISearchQuery) query);
	}

	public static IStatus runQueryInForeground(
			final IProgressService progressService, final Object query) {
		return NewSearchUI.runQueryInForeground(progressService,
				(ISearchQuery) query);
	}

	public static ErlangExternalFunctionCallRef getRefFromOpenRes(
			final OpenResult res) {
		if (!res.isExternalCall()) {
			return null;
		}
		return new ErlangExternalFunctionCallRef(res.getName(), res.getFun(),
				res.getArity());
	}

	public static ErlangExternalFunctionCallRef getRefFromErlElement(
			final IErlElement element) {
		if (element instanceof IErlFunction) {
			final IErlFunction function = (IErlFunction) element;
			return new ErlangExternalFunctionCallRef(ErlideUtil
					.withoutExtension(function.getModule().getName()), function
					.getName(), function.getArity());
		} else if (element instanceof IErlFunctionClause) {
			final IErlFunctionClause clause = (IErlFunctionClause) element;
			getRefFromErlElement(clause.getParent());
		}
		return null;
	}

	public static boolean isLineDelimiterChar(final char ch) {
		return ch == '\n' || ch == '\r';
	}

	public static IErlModule getModule(final IErlElement key) {
		if (key instanceof IErlFunction) {
			return (IErlModule) key.getParent();
		} else if (key instanceof IErlFunctionClause) {
			return (IErlModule) key.getParent().getParent();
		}
		return null;
	}

	// public static String toString(final IWorkingSet[] workingSets) {
	// Arrays.sort(workingSets, new Comparator<IWorkingSet>() {
	// private final Collator fCollator = Collator.getInstance();
	//
	// public int compare(final IWorkingSet o1, final IWorkingSet o2) {
	// final String name1 = o1.getLabel();
	// final String name2 = o2.getLabel();
	//
	// return fCollator.compare(name1, name2);
	// }
	// });
	// String result = "";
	// if (workingSets != null && workingSets.length > 0) {
	// boolean firstFound = false;
	// for (int i = 0; i < workingSets.length; i++) {
	// final String workingSetLabel = workingSets[i].getLabel();
	// if (firstFound) {
	// result = String.format("{0}{1}", new Object[] { result,
	// workingSetLabel });
	// } else {
	// result = workingSetLabel;
	// firstFound = true;
	// }
	// }
	// }
	// return result;
	// }
	//
	// // LRU working sets
	// public static final int LRU_WORKINGSET_LIST_SIZE = 3;
	// private static LRUWorkingSetsList fgLRUWorkingSets;
	//
	// // Settings store
	// private static final String DIALOG_SETTINGS_KEY =
	// "JavaElementSearchActions"; //$NON-NLS-1$
	// private static final String STORE_LRU_WORKING_SET_NAMES =
	// "lastUsedWorkingSetNames"; //$NON-NLS-1$
	//
	// private static final String BIN_PRIM_CONST_WARN_DIALOG_ID =
	// "BinaryPrimitiveConstantWarningDialog"; //$NON-NLS-1$
	//
	// public static boolean isSearchPlugInActivated() {
	// return Platform.getBundle("org.eclipse.search").getState() ==
	// Bundle.ACTIVE; //$NON-NLS-1$
	// }
	//
	// private static IDialogSettings getDialogStoreSection() {
	// IDialogSettings settingsStore = ErlangPlugin.getDefault()
	// .getDialogSettings().getSection(DIALOG_SETTINGS_KEY);
	// if (settingsStore == null) {
	// settingsStore = JavaPlugin.getDefault().getDialogSettings()
	// .addNewSection(DIALOG_SETTINGS_KEY);
	// }
	// return settingsStore;
	// }
	//
	// public static LRUWorkingSetsList getLRUWorkingSets() {
	// if (fgLRUWorkingSets == null) {
	// restoreState();
	// }
	// return fgLRUWorkingSets;
	// }
	//
	// private static void restoreState() {
	// fgLRUWorkingSets = new LRUWorkingSetsList(LRU_WORKINGSET_LIST_SIZE);
	// final IDialogSettings settingsStore = getDialogStoreSection();
	//
	// boolean foundLRU = false;
	// for (int i = LRU_WORKINGSET_LIST_SIZE - 1; i >= 0; i--) {
	// final String[] lruWorkingSetNames = settingsStore
	// .getArray(STORE_LRU_WORKING_SET_NAMES + i);
	// if (lruWorkingSetNames != null) {
	// final Set workingSets = new HashSet(2);
	// for (int j = 0; j < lruWorkingSetNames.length; j++) {
	// final IWorkingSet workingSet = PlatformUI.getWorkbench()
	// .getWorkingSetManager().getWorkingSet(
	// lruWorkingSetNames[j]);
	// if (workingSet != null) {
	// workingSets.add(workingSet);
	// }
	// }
	// foundLRU = true;
	// if (!workingSets.isEmpty()) {
	// fgLRUWorkingSets.add((IWorkingSet[]) workingSets
	// .toArray(new IWorkingSet[workingSets.size()]));
	// }
	// }
	// }
	// if (!foundLRU) {
	// // try old preference format
	// restoreFromOldFormat();
	// }
	// }
}
