package org.erlide.ui.internal.search;

import java.text.MessageFormat;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.DecoratingStyledCellLabelProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
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
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.services.builder.MarkerUtils;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.util.EditorUtility;

public class ErlangSearchResultPage extends AbstractTextSearchViewPage {

    @Override
    protected void handleOpen(final OpenEvent event) {
        super.handleOpen(event);
    }

    public static class DecoratorIgnoringViewerSorter extends ViewerComparator {
        private final ILabelProvider fLabelProvider;

        public DecoratorIgnoringViewerSorter(final ILabelProvider labelProvider) {
            fLabelProvider = labelProvider;
        }

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

    private static final String[] SHOW_IN_TARGETS = new String[] { IPageLayout.ID_PROJECT_EXPLORER };
    private static final IShowInTargetList SHOW_IN_TARGET_LIST = new IShowInTargetList() {
        @Override
        @SuppressWarnings("synthetic-access")
        public String[] getShowInTargetIds() {
            return SHOW_IN_TARGETS;
        }
    };
    private SearchResultLabelProvider innerLabelProvider = null;

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
        viewer.setLabelProvider(new DecoratingStyledCellLabelProvider(
                getInnerLabelProvider(), PlatformUI.getWorkbench()
                        .getDecoratorManager().getLabelDecorator(), null));
        viewer.setContentProvider(new ErlangSearchTableContentProvider(this));
        viewer.setComparator(new DecoratorIgnoringViewerSorter(
                getInnerLabelProvider()));
        fContentProvider = (IErlSearchContentProvider) viewer
                .getContentProvider();
        addDragAdapters(viewer);
    }

    private SearchResultLabelProvider getInnerLabelProvider() {
        if (innerLabelProvider == null) {
            innerLabelProvider = new SearchResultLabelProvider(this,
                    fCurrentSortOrder, false);
        }
        return innerLabelProvider;
    }

    @Override
    protected void configureTreeViewer(final TreeViewer viewer) {
        viewer.setUseHashlookup(true);
        innerLabelProvider = new SearchResultLabelProvider(this,
                SearchResultLabelProvider.SHOW_LABEL, true);
        viewer.setLabelProvider(new DecoratingStyledCellLabelProvider(
                innerLabelProvider, PlatformUI.getWorkbench()
                        .getDecoratorManager().getLabelDecorator(), null));
        viewer.setContentProvider(new ErlangSearchTreeContentProvider(viewer,
                this));
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
            final IErlModule module = ese.getModule();
            final IEditorPart editor = EditorUtility.openInEditor(module);
            if (offset != 0) {
                if (editor instanceof ErlangEditor) {
                    final ErlangEditor ee = (ErlangEditor) editor;
                    ee.selectAndReveal(offset, length);
                } else if (editor != null) {
                    showWithMarker(editor, module, offset, length);
                }
            }
        }
    }

    private void showWithMarker(final IEditorPart editor,
            final IErlModule module, final int offset, final int length)
            throws PartInitException {
        IMarker marker = null;
        try {
            marker = MarkerUtils.createSearchResultMarker(module,
                    NewSearchUI.SEARCH_MARKER, offset, length);
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
            fCurrentSortOrder = SearchResultLabelProvider.SHOW_LABEL_PATH;
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
                    return MessageFormat.format(format, label,
                            Integer.valueOf(itemCount),
                            Integer.valueOf(fileCount));
                }
            }
        }
        return label;
    }

}
