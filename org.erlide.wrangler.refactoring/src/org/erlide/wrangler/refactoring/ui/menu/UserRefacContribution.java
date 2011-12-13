package org.erlide.wrangler.refactoring.ui.menu;

import java.util.HashMap;
import java.util.List;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;
import org.erlide.wrangler.refactoring.backend.UserRefactoringInfo;
import org.erlide.wrangler.refactoring.backend.UserRefactoringsManager;

/**
 * Menu contribution - committed refactoring menu items (those from the
 * repository)
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class UserRefacContribution extends CompoundContributionItem {

    public UserRefacContribution() {
        super();
    }

    public UserRefacContribution(final String id) {
        super(id);
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    protected IContributionItem[] getContributionItems() {

        final List<UserRefactoringInfo> refacs = UserRefactoringsManager
                .getInstance().getElementary();
        final IContributionItem[] items = new IContributionItem[refacs.size()];

        int i = 0;
        for (final UserRefactoringInfo info : refacs) {
            final CommandContributionItemParameter param = new CommandContributionItemParameter(
                    PlatformUI.getWorkbench(), info.getCallback(),
                    "org.erlide.wrangler.refactoring.gen_refac",
                    CommandContributionItem.STYLE_PUSH);
            param.label = info.getLabel();
            param.parameters = new HashMap();
            param.parameters.put(
                    "org.erlide.wrangler.refactoring.gen_refac.callback",
                    info.getCallback());
            param.parameters.put(
                    "org.erlide.wrangler.refactoring.gen_refac.name",
                    info.getLabel());
            items[i] = new CommandContributionItem(param);
            i++;
        }

        return items;
    }

}
