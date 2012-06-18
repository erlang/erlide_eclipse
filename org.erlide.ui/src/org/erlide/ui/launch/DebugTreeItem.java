package org.erlide.ui.launch;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.swt.widgets.Control;
import org.erlide.core.internal.model.erlang.ErlExternalReferenceEntry;
import org.erlide.core.internal.model.erlang.ErlExternalReferenceEntryList;
import org.erlide.core.internal.model.erlang.ErlOtpExternalReferenceEntryList;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlFolder;
import org.erlide.core.model.root.IOpenable;
import org.erlide.core.model.root.IParent;
import org.erlide.jinterface.ErlLogger;

public class DebugTreeItem {
    final IErlElement item;
    final DebugTreeItem parent;
    final List<DebugTreeItem> children = new ArrayList<DebugTreeItem>();

    public DebugTreeItem(final IErlElement item, final DebugTreeItem parent) {
        this.item = item;
        this.parent = parent;
    }

    public DebugTreeItem getParent() {
        return parent;
    }

    public IErlElement getItem() {
        return item;
    }

    public List<DebugTreeItem> getChildren() {
        return children;
    }

    public boolean areChildrenChecked(final CheckboxTreeViewer tree) {
        for (final DebugTreeItem i : children) {
            if (!tree.getChecked(i) || tree.getGrayed(i)) {
                return false;
            }
        }
        return true;
    }

    public boolean areChildrenUnchecked(final CheckboxTreeViewer tree) {
        for (final DebugTreeItem i : children) {
            if (tree.getChecked(i) || tree.getGrayed(i)) {
                return false;
            }
        }
        return true;
    }

    boolean addAllErlangModules(final IErlElement elem) {
        if (elem instanceof IErlModule) {
            children.add(new DebugTreeItem(elem, this));
            return true;
        } else if (elem instanceof ErlOtpExternalReferenceEntryList) {
            return false;
        } else if (elem instanceof ErlExternalReferenceEntryList) {
            return false;
        } else if (elem instanceof ErlExternalReferenceEntry) {
            return false;
        } else if (elem instanceof IParent) {
            try {
                if (elem instanceof IErlFolder) {
                    final IErlFolder f = (IErlFolder) elem;
                    if (!f.isSourcePathParent()) {
                        return false;
                    }
                }
                if (elem instanceof IOpenable) {
                    final IOpenable o = (IOpenable) elem;
                    o.open(null);
                }
                final DebugTreeItem dti = new DebugTreeItem(elem, this);
                final IParent p = (IParent) elem;
                boolean addedAny = false;
                for (final IErlElement i : p.getChildren()) {
                    addedAny |= dti.addAllErlangModules(i);
                }
                if (addedAny) {
                    children.add(dti);
                }
                return true;
            } catch (final ErlModelException e) {
                ErlLogger.warn(e);
            }
        }
        return false;
    }

    private void setGrayChecked(final CheckboxTreeViewer checkboxTreeViewer,
            final boolean grayed, final boolean checked) {
        checkboxTreeViewer.setGrayed(this, grayed);
        checkboxTreeViewer.setChecked(this, checked);
    }

    private void updateMenuCategoryCheckedState(
            final CheckboxTreeViewer checkboxTreeViewer) {
        if (areChildrenUnchecked(checkboxTreeViewer)) {
            setGrayChecked(checkboxTreeViewer, false, false);
        } else if (areChildrenChecked(checkboxTreeViewer)) {
            setGrayChecked(checkboxTreeViewer, false, true);
        } else {
            setGrayChecked(checkboxTreeViewer, true, true);
        }
        if (getParent() != null) {
            getParent().updateMenuCategoryCheckedState(checkboxTreeViewer);
        }
    }

    public void setChecked(final CheckboxTreeViewer checkboxTreeViewer,
            final Collection<IErlModule> modules) {
        final Control tree = checkboxTreeViewer.getControl();
        tree.setRedraw(false);
        try {
            doSetChecked(checkboxTreeViewer, modules);
        } finally {
            tree.setRedraw(true);
        }
    }

    private void doSetChecked(final CheckboxTreeViewer checkboxTreeViewer,
            final Collection<IErlModule> modules) {
        setGrayChecked(checkboxTreeViewer, false, modules.contains(item));
        for (final DebugTreeItem c : children) {
            c.setChecked(checkboxTreeViewer, modules);
            c.getParent().updateMenuCategoryCheckedState(checkboxTreeViewer);
        }
    }
}
