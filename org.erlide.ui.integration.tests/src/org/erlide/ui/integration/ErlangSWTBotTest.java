package org.erlide.ui.integration;

import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.junit.AfterClass;

public class ErlangSWTBotTest {

    protected static SWTWorkbenchBot bot;

    protected static boolean isProjectCreated(final String name) {
        try {
            final SWTBotView packageExplorer = getNavigator();
            final SWTBotTree tree = packageExplorer.bot().tree();
            tree.getTreeItem(name);
            return true;
        } catch (final WidgetNotFoundException e) {
            return false;
        }
    }

    protected static SWTBotView getNavigator() {
        final SWTBotView view = bot.viewByTitle("Erlang Navigator");
        return view;
    }

    protected static void waitForShellToDisappear(final String title) {
        try {
            while (bot.shell(title).isActive()) {
                // wait
            }
        } catch (final WidgetNotFoundException e) {
            // Ignore
        }
    }

    @AfterClass
    public static void sleep() {
        bot.sleep(2000);
    }

}
