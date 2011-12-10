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
 * Menu contribution for generating menu items for user's own refactorings
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class UserMyRefacContribution extends CompoundContributionItem {

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    protected IContributionItem[] getContributionItems() {
        final List<UserRefactoringInfo> refacs = UserRefactoringsManager
                .getInstance().getMyElementary();
        if (refacs.size() == 0) {
            final CommandContributionItemParameter param = new CommandContributionItemParameter(
                    PlatformUI.getWorkbench(), null,
                    "org.erlide.wrangler.refactoring.empty",
                    CommandContributionItem.STYLE_PUSH);
            param.label = "<Empty>";
            final CommandContributionItem item = new CommandContributionItem(
                    param);
            return new IContributionItem[] { item };
        }
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
