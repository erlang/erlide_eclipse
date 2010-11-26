package org.erlide.ui.internal.search;

import org.eclipse.ui.IViewPart;
import org.eclipse.ui.actions.ActionGroup;
import org.erlide.ui.actions.CompositeActionGroup;
import org.erlide.ui.actions.OpenEditorActionGroup;

public class NewErlSearchActionGroup extends CompositeActionGroup {

    public NewErlSearchActionGroup(final IViewPart part) {
        OpenViewActionGroup openViewActionGroup;
        setGroups(new ActionGroup[] { new OpenEditorActionGroup(part),
                openViewActionGroup = new OpenViewActionGroup(part) });
        openViewActionGroup.containsShowInMenu(false);
    }

}
