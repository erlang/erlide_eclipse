package org.erlide.ui.search;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.DecoratingLabelProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.OpenEvent;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.search.ui.ISearchResultViewPart;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.search.ui.text.AbstractTextSearchResult;
import org.eclipse.search.ui.text.AbstractTextSearchViewPage;
import org.eclipse.search.ui.text.Match;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.IPageSite;
import org.eclipse.ui.part.IShowInTargetList;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.core.erlang.util.ResourceUtil;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.outline.ErlangElementImageProvider;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.internal.search.NewErlSearchActionGroup;

public class ErlangSearchResultPage extends AbstractTextSearchViewPage {

	@Override
	protected void handleOpen(final OpenEvent event) {
		super.handleOpen(event);
	}

	public class TableContentProvider implements IStructuredContentProvider,
			IErlSearchContentProvider {

		private final Object[] EMPTY_ARR = new Object[0];

		private final ErlangSearchResultPage fPage;
		private ErlangSearchResult fResult;

		TableContentProvider(final ErlangSearchResultPage page) {
			fPage = page;
		}

		public void dispose() {
			// nothing to do
		}

		public void inputChanged(final Viewer viewer, final Object oldInput,
				final Object newInput) {
			if (newInput instanceof ErlangSearchResult) {
				fResult = (ErlangSearchResult) newInput;
			}
		}

		public void elementsChanged(final Object[] updatedElements) {
			final TableViewer viewer = getViewer();
			final int elementLimit = getElementLimit();
			final boolean tableLimited = elementLimit != -1;
			for (int i = 0; i < updatedElements.length; i++) {
				if (fResult.getMatchCount(updatedElements[i]) > 0) {
					if (viewer.testFindItem(updatedElements[i]) != null) {
						viewer.update(updatedElements[i], null);
					} else {
						if (!tableLimited
								|| viewer.getTable().getItemCount() < elementLimit) {
							viewer.add(updatedElements[i]);
						}
					}
				} else {
					viewer.remove(updatedElements[i]);
				}
			}
		}

		public Object[] getElements(final Object inputElement) {
			if (inputElement instanceof ErlangSearchResult) {
				final ErlangSearchResult esr = (ErlangSearchResult) inputElement;
				final int elementLimit = getElementLimit();
				final Object[] elements = esr.getElements();
				if (elementLimit != -1 && elements.length > elementLimit) {
					final Object[] shownElements = new Object[elementLimit];
					System.arraycopy(elements, 0, shownElements, 0,
							elementLimit);
					return shownElements;
				}
				return elements;
			}
			return EMPTY_ARR;
		}

		public void clear() {
			getViewer().refresh();
		}

		private int getElementLimit() {
			return fPage.getElementLimit().intValue();
		}

		private TableViewer getViewer() {
			return (TableViewer) fPage.getViewer();
		}
	}

	public class SearchResultLabelProvider extends LabelProvider {

		public static final int SHOW_LABEL = 1;
		public static final int SHOW_LABEL_PATH = 2;
		public static final int SHOW_PATH_LABEL = 3;
		public static final int SHOW_PATH = 4;

		private final ErlangElementImageProvider fImageProvider;
		private final AbstractTextSearchViewPage fPage;

		private int fOrder;

		// private final String[] fArgs = new String[2];

		public SearchResultLabelProvider(final AbstractTextSearchViewPage page,
				final int orderFlag) {
			fImageProvider = new ErlangElementImageProvider();
			fOrder = orderFlag;
			fPage = page;
		}

		public void setOrder(final int orderFlag) {
			fOrder = orderFlag;
		}

		public int getOrder() {
			return fOrder;
		}

		// protected final String getLabelWithCounts(final Object element,
		// final String elementName) {
		// final int matchCount = fPage.getDisplayedMatchCount(element);
		// if (matchCount < 2) {
		// if (matchCount == 1) {
		// return MessageFormat.format("{0} (1 match)", elementName);
		// } else {
		// return elementName;
		// }
		// } else {
		// return MessageFormat.format("{0} ({1} matches)", elementName,
		// String.valueOf(matchCount));
		// }
		// }

		@Override
		public String getText(final Object element) {
			final String text;
			if (element instanceof String) { // Module
				final String s = (String) element;
				text = new Path(s).lastSegment();
			} else if (element instanceof ErlangSearchElement) {
				final ErlangSearchElement ese = (ErlangSearchElement) element;
				final String clauseHead = ese.getClauseHead();
				if (clauseHead != null) {
					text = ese.getFunction().name + clauseHead;
				} else {
					text = ese.getFunction().getNameWithArity();
				}
			} else if (element instanceof ErlangFunction) {
				final ErlangFunction f = (ErlangFunction) element;
				text = f.getNameWithArity();
			} else {
				text = null;
			}
			int matchCount = 0;
			final AbstractTextSearchResult result = fPage.getInput();
			if (result != null) {
				matchCount = fPage.getDisplayedMatchCount(element);
			}
			if (matchCount == 0) {
				return text;
			} else if (matchCount == 1) {
				return MessageFormat.format("{0} 1 match", text);
			}
			final String format = "{0} ({1} matches)";
			return MessageFormat.format(format, text, Integer
					.valueOf(matchCount));
		}

		@Override
		public Image getImage(final Object element) {
			// module - String
			// function - ErlangFunction
			// clause - ClauseHead
			// occurence - ModuleLineFunctionArityRef
			Kind kind = Kind.ERROR;
			if (element instanceof String) {
				kind = Kind.MODULE;
			} else if (element instanceof ErlangSearchElement) {
				final ErlangSearchElement ese = (ErlangSearchElement) element;
				if (ese.getClauseHead() == null) {
					kind = Kind.FUNCTION;
				} else {
					kind = Kind.CLAUSE;
				}
			} else if (element instanceof ErlangFunction) {
				kind = Kind.FUNCTION;
			}
			// if (element instanceof ModuleLineFunctionArityRef) {
			// final ModuleLineFunctionArityRef mlfar =
			// (ModuleLineFunctionArityRef) element;
			// ErlLogger.debug("fixa");// TODO fixa
			// e = null;
			// } else {
			// e = element;
			// }
			return fImageProvider.getImageLabel(ErlangElementImageProvider
					.getImageDescriptionFromKind(kind));
		}

		@Override
		public void dispose() {
			super.dispose();
		}

		@Override
		public boolean isLabelProperty(final Object element,
				final String property) {
			return super.isLabelProperty(element, property);
			// return fLabelProvider.isLabelProperty(element, property);
		}

		@Override
		public void removeListener(final ILabelProviderListener listener) {
			super.removeListener(listener);
			// fLabelProvider.removeListener(listener);
		}

		@Override
		public void addListener(final ILabelProviderListener listener) {
			super.addListener(listener);
			// fLabelProvider.addListener(listener);
		}
	}

	public class TreeContentProvider implements ITreeContentProvider,
			IErlSearchContentProvider {

		private final Object[] EMPTY_ARR = new Object[0];

		// private AbstractTextSearchResult fResult;
		private final AbstractTreeViewer fTreeViewer;
		private final Map<Object, List<Object>> childMap;
		private final Map<Object, Object> parentMap;
		private final List<String> moduleNames;
		private ErlangSearchResult fResult;

		private TreeContentProvider(final AbstractTreeViewer viewer) {
			fTreeViewer = viewer;
			childMap = new HashMap<Object, List<Object>>();
			parentMap = new HashMap<Object, Object>();
			moduleNames = new ArrayList<String>();
			// modules = new ArrayList<IErlModule>();
		}

		public Object[] getElements(final Object inputElement) {
			return moduleNames.toArray();
		}

		public void dispose() {
			// nothing to do
		}

		public void inputChanged(final Viewer viewer, final Object oldInput,
				final Object newInput) {
			if (newInput instanceof ErlangSearchResult) {
				fResult = (ErlangSearchResult) newInput;
				initialize(fResult.getResult());
			}
		}

		private void addChild(final Object parent, final Object child) {
			if (!parentMap.containsKey(child)) {
				parentMap.put(child, parent);
			}
			if (childMap.containsKey(parent)) {
				final List<Object> children = childMap.get(parent);
				if (!children.contains(child)) {
					children.add(child);
				}
			} else {
				final List<Object> children = new ArrayList<Object>(1);
				children.add(child);
				childMap.put(parent, children);
			}
		}

		protected synchronized void initialize(
				final List<ErlangSearchElement> eses) {
			if (eses == null) {
				return;
			}
			moduleNames.clear();
			parentMap.clear();
			childMap.clear();
			for (final ErlangSearchElement ese : eses) {
				final String moduleName = ese.getModuleName();
				if (!moduleNames.contains(moduleName)) {
					moduleNames.add(moduleName);
				}
				final ErlangFunction function = ese.getFunction();
				final String clauseHead = ese.getClauseHead();
				if (clauseHead != null) {
					addChild(moduleName, function);
					addChild(function, ese);
					// addChild(function, clauseHead);
					// addChild(clauseHead, ese);
				} else {
					addChild(moduleName, ese);
					// addChild(function, ese);
				}
			}

		}

		public Object[] getChildren(final Object parentElement) {
			final List<Object> l = childMap.get(parentElement);
			if (l == null) {
				return EMPTY_ARR;
			}
			return l.toArray();
		}

		public boolean hasChildren(final Object element) {
			return childMap.containsKey(element);
		}

		// @Override
		// public synchronized void elementsChanged(final Object[]
		// updatedElements) {
		// for (int i = 0; i < updatedElements.length; i++) {
		// if (fResult.getMatchCount(updatedElements[i]) > 0) {
		// insert(updatedElements[i], true);
		// } else {
		// remove(updatedElements[i], true);
		// }
		// }
		// }

		// @Override
		// public void clear() {
		// initialize(fResult);
		// fTreeViewer.refresh();
		// }

		public Object getParent(final Object element) {
			return parentMap.get(element);
		}

		public void clear() {
			initialize(fResult.getResult());
			fTreeViewer.refresh();
		}

		public void elementsChanged(final Object[] updatedElements) {
			// FIXME ska det vara så här? eller ska vi kolla med updatedElements
			clear();
		}
	}

	public static class DecoratorIgnoringViewerSorter extends ViewerComparator {
		private final ILabelProvider fLabelProvider;

		public DecoratorIgnoringViewerSorter(final ILabelProvider labelProvider) {
			fLabelProvider = labelProvider;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.jface.viewers.ViewerComparator#category(java.lang.Object)
		 */
		@Override
		public int category(final Object element) {
			if (element instanceof IContainer) {
				return 1;
			}
			return 2;
		}

		@SuppressWarnings("unchecked")
		@Override
		public int compare(final Viewer viewer, final Object e1, final Object e2) {
			final int cat1 = category(e1);
			final int cat2 = category(e2);

			if (cat1 != cat2) {
				return cat1 - cat2;
			}

			String name1 = fLabelProvider.getText(e1);
			String name2 = fLabelProvider.getText(e2);
			if (name1 == null) {
				name1 = "";//$NON-NLS-1$
			}
			if (name2 == null) {
				name2 = "";//$NON-NLS-1$
			}
			return getComparator().compare(name1, name2);
		}
	}

	private static final String KEY_SORTING = "org.eclipse.search.resultpage.sorting"; //$NON-NLS-1$
	private static final String KEY_LIMIT = "org.eclipse.search.resultpage.limit"; //$NON-NLS-1$

	private static final int DEFAULT_ELEMENT_LIMIT = 1000;

	private ActionGroup fActionGroup;
	private int fCurrentSortOrder;
	// private final SortAction fSortByNameAction;
	// private final SortAction fSortByPathAction;
	private IErlSearchContentProvider fContentProvider;

	// private final EditorOpener fEditorOpener = new EditorOpener();

	private static final String[] SHOW_IN_TARGETS = new String[] { IPageLayout.ID_RES_NAV };
	private static final IShowInTargetList SHOW_IN_TARGET_LIST = new IShowInTargetList() {
		@SuppressWarnings("synthetic-access")
		public String[] getShowInTargetIds() {
			return SHOW_IN_TARGETS;
		}
	};

	public ErlangSearchResultPage() {
		// fSortByNameAction = new SortAction(
		// SearchMessages.FileSearchPage_sort_name_label, this,
		// FileLabelProvider.SHOW_LABEL_PATH);
		// fSortByPathAction = new SortAction(
		// SearchMessages.FileSearchPage_sort_path_label, this,
		// FileLabelProvider.SHOW_PATH_LABEL);

		setElementLimit(Integer.valueOf(DEFAULT_ELEMENT_LIMIT));
	}

	@Override
	public void setElementLimit(final Integer elementLimit) {
		super.setElementLimit(elementLimit);
		final int limit = elementLimit.intValue();
		getSettings().put(KEY_LIMIT, limit);
	}

	@Override
	public StructuredViewer getViewer() {
		return super.getViewer();
	}

	private void addDragAdapters(final StructuredViewer viewer) {
		// final Transfer[] transfers = new Transfer[] { ResourceTransfer
		// .getInstance() };
		// final int ops = DND.DROP_COPY | DND.DROP_LINK;
		// viewer.addDragSupport(ops, transfers, new
		// ResourceTransferDragAdapter(
		// viewer));
	}

	@Override
	protected void configureTableViewer(final TableViewer viewer) {
		viewer.setUseHashlookup(true);
		final SearchResultLabelProvider innerLabelProvider = new SearchResultLabelProvider(
				this, fCurrentSortOrder);
		viewer.setLabelProvider(new DecoratingLabelProvider(innerLabelProvider,
				PlatformUI.getWorkbench().getDecoratorManager()
						.getLabelDecorator()));
		viewer.setContentProvider(new TableContentProvider(this));
		viewer.setComparator(new DecoratorIgnoringViewerSorter(
				innerLabelProvider));
		fContentProvider = (IErlSearchContentProvider) viewer
				.getContentProvider();
		addDragAdapters(viewer);
	}

	@Override
	protected void configureTreeViewer(final TreeViewer viewer) {
		viewer.setUseHashlookup(true);
		final SearchResultLabelProvider innerLabelProvider = new SearchResultLabelProvider(
				this, SearchResultLabelProvider.SHOW_LABEL);
		viewer.setLabelProvider(new DecoratingLabelProvider(innerLabelProvider,
				PlatformUI.getWorkbench().getDecoratorManager()
						.getLabelDecorator()));
		viewer.setContentProvider(new TreeContentProvider(viewer));
		viewer.setComparator(new DecoratorIgnoringViewerSorter(
				innerLabelProvider));
		fContentProvider = (IErlSearchContentProvider) viewer
				.getContentProvider();
		addDragAdapters(viewer);
	}

	@Override
	protected void showMatch(final Match match, final int offset,
			final int length, final boolean activate) throws PartInitException {
		final Object element = match.getElement();
		if (element instanceof ErlangSearchElement) {
			final ErlangSearchElement ese = (ErlangSearchElement) element;
			final IFile file = ResourceUtil.getFileFromLocation(ese
					.getModuleName());
			try {
				final IEditorPart editor = EditorUtility.openInEditor(file,
						activate);
				if (offset != 0) {
					if (editor instanceof ErlangEditor) {
						final ErlangEditor ee = (ErlangEditor) editor;
						final IDocument doc = ee.getDocument();
						int lineOffset;
						try {
							lineOffset = doc.getLineOffset(offset);
							ee.selectAndReveal(lineOffset, length);
						} catch (final BadLocationException e) {
							e.printStackTrace();
						}
					} else if (editor != null) {
						showWithMarker(editor, file, offset, length);
					}
				}
			} catch (final ErlModelException e) {
			}
		}
	}

	private void showWithMarker(final IEditorPart editor, final IFile file,
			final int offset, final int length) throws PartInitException {
		IMarker marker = null;
		try {
			marker = file.createMarker(NewSearchUI.SEARCH_MARKER);
			final HashMap<String, Integer> attributes = new HashMap<String, Integer>(
					4);
			attributes.put(IMarker.CHAR_START, Integer.valueOf(offset));
			attributes.put(IMarker.CHAR_END, Integer.valueOf(offset + length));
			marker.setAttributes(attributes);
			IDE.gotoMarker(editor, marker);
		} catch (final CoreException e) {
			throw new PartInitException(
					"SearchMessages.FileSearchPage_error_marker", e);
		} finally {
			if (marker != null) {
				try {
					marker.delete();
				} catch (final CoreException e) {
					// ignore
				}
			}
		}
	}

	protected boolean hasChildren(final Object element) {
		final ITreeContentProvider contentProvider = (ITreeContentProvider) getViewer()
				.getContentProvider();
		return contentProvider.hasChildren(element);
	}

	@Override
	protected void fillContextMenu(final IMenuManager mgr) {
		super.fillContextMenu(mgr);
		addSortActions(mgr);
		fActionGroup.setContext(new ActionContext(getSite()
				.getSelectionProvider().getSelection()));
		fActionGroup.fillContextMenu(mgr);
	}

	private void addSortActions(final IMenuManager mgr) {
		if (getLayout() != FLAG_LAYOUT_FLAT) {
			return;
		}
		// final MenuManager sortMenu = new MenuManager(
		// "Sort by");
		// sortMenu.add(fSortByNameAction);
		// sortMenu.add(fSortByPathAction);
		//
		// fSortByNameAction.setChecked(fCurrentSortOrder == fSortByNameAction
		// .getSortOrder());
		// fSortByPathAction.setChecked(fCurrentSortOrder == fSortByPathAction
		// .getSortOrder());
		//
		// mgr.appendToGroup(IContextMenuConstants.GROUP_VIEWER_SETUP,
		// sortMenu);
	}

	@Override
	public void setViewPart(final ISearchResultViewPart part) {
		super.setViewPart(part);
		fActionGroup = new NewErlSearchActionGroup(part);
	}

	@Override
	public void init(final IPageSite site) {
		super.init(site);
		// final IMenuManager menuManager =
		// site.getActionBars().getMenuManager();
		// menuManager.appendToGroup(IContextMenuConstants.GROUP_PROPERTIES,
		// new OpenSearchPreferencesAction());
	}

	@Override
	public void dispose() {
		fActionGroup.dispose();
		super.dispose();
	}

	@Override
	protected void elementsChanged(final Object[] objects) {
		if (fContentProvider != null) {
			fContentProvider.elementsChanged(objects);
		}
	}

	@Override
	protected void clear() {
		if (fContentProvider != null) {
			fContentProvider.clear();
		}
	}

	public void setSortOrder(final int sortOrder) {
		fCurrentSortOrder = sortOrder;
		// final DecoratingLabelProvider lpWrapper = (DecoratingLabelProvider)
		// getViewer()
		// .getLabelProvider();
		// FIXME ((LabelProvider)
		// lpWrapper.getLabelProvider()).setOrder(sortOrder);
		getViewer().refresh();
		getSettings().put(KEY_SORTING, fCurrentSortOrder);
	}

	@Override
	public void restoreState(final IMemento memento) {
		super.restoreState(memento);
		try {
			fCurrentSortOrder = getSettings().getInt(KEY_SORTING);
		} catch (final NumberFormatException e) {
			// FIXME fCurrentSortOrder = fSortByNameAction.getSortOrder();
		}
		int elementLimit = DEFAULT_ELEMENT_LIMIT;
		try {
			elementLimit = getSettings().getInt(KEY_LIMIT);
		} catch (final NumberFormatException e) {
		}
		if (memento != null) {
			Integer value = memento.getInteger(KEY_SORTING);
			if (value != null) {
				fCurrentSortOrder = value.intValue();
			}

			value = memento.getInteger(KEY_LIMIT);
			if (value != null) {
				elementLimit = value.intValue();
			}
		}
		setElementLimit(Integer.valueOf(elementLimit));
	}

	@Override
	public void saveState(final IMemento memento) {
		super.saveState(memento);
		memento.putInteger(KEY_SORTING, fCurrentSortOrder);
		memento.putInteger(KEY_LIMIT, getElementLimit().intValue());
	}

	public Object getAdapter(final Class<?> adapter) {
		if (IShowInTargetList.class.equals(adapter)) {
			return SHOW_IN_TARGET_LIST;
		}
		return null;
	}

	@Override
	public String getLabel() {
		final String label = super.getLabel();
		final StructuredViewer viewer = getViewer();
		if (viewer instanceof TableViewer) {
			final TableViewer tv = (TableViewer) viewer;

			final AbstractTextSearchResult result = getInput();
			if (result != null) {
				final int itemCount = ((IStructuredContentProvider) tv
						.getContentProvider()).getElements(getInput()).length;
				final int fileCount = getInput().getElements().length;
				if (itemCount < fileCount) {
					final String format = "{0} (showing {1} of {2} files)";
					return MessageFormat.format(format, label, new Integer(
							itemCount), new Integer(fileCount));
				}
			}
		}
		return label;
	}

}
