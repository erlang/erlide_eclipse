package org.erlide.ui.integration;

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.widgetOfType;
import static org.junit.Assert.fail;
import junit.framework.Assert;

import org.eclipse.swt.widgets.Button;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEclipseEditor;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.waits.Conditions;
import org.eclipse.swtbot.swt.finder.waits.DefaultCondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotCheckBox;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(SWTBotJunit4ClassRunner.class)
public class ProjectManagementTest extends ErlangSWTBotTest {

    @BeforeClass
    public static void beforeClass() {
        bot = new SWTWorkbenchBot();
        bot.perspectiveByLabel("Erlang").activate();
        bot.menu("Project", 1).menu("Build Automatically").click();
    }

    @AfterClass
    public static void deleteProject() {
        final SWTBotView packageExplorer = getNavigator();
        final SWTBotTree tree = packageExplorer.bot().tree();
        packageExplorer.show();
        tree.select("MyFirstProject");
        bot.menu("Edit").menu("Delete").click();
        final SWTBotShell ashell = bot.shell("Delete Resources");
        ashell.activate();
        bot.checkBox("Delete project contents on disk (cannot be undone)")
                .select();
        bot.button("OK").click();
        bot.waitUntil(org.eclipse.swtbot.swt.finder.waits.Conditions
                .shellCloses(ashell));
    }

    @Test
    public void canCreateANewErlangProject() throws Exception {
        bot.menu("File").menu("New").menu("Project...").click();

        final SWTBotShell shell = bot.shell("New Project");
        shell.activate();
        bot.tree().expandNode("Erlang").select("Erlang Project");
        bot.button("Next >").click();

        bot.textWithLabel("Project name:").setText("MyFirstProject");

        bot.button("Finish").click();
        // FIXME: assert that the project is actually created, for later
        Assert.assertTrue("Project could not be created",
                isProjectCreated("MyFirstProject"));

    }

    @Test
    public void canCreateANewModule() {
        bot.toolbarDropDownButtonWithTooltip("New").menuItem("Module").click();
        bot.shell("New Erlang module").activate();
        bot.textWithLabel("Container:").setText("MyFirstProject/src");
        bot.textWithLabel("Module name:").setText("a_module");
        bot.button("Finish").click();
        waitForShellToDisappear("New Erlang module");

        try {
            final SWTBotView packageExplorer = getNavigator();
            final SWTBotTree tree = packageExplorer.bot().tree();
            tree.expandNode("MyFirstProject", true);
            tree.getTreeItem("MyFirstProject").getNode("src")
                    .getNode("a_module.erl").doubleClick();
            // bot.activeEditor().toTextEditor().setText("-module(a_module).\n");
            bot.activeEditor().save();
        } catch (final WidgetNotFoundException e) {
            fail("Module 'a_module.erl' has NOT been created " + e.getMessage());
        }
    }

    @Test
    public void canCompileModule() {
        try {
            final SWTBotView packageExplorer = getNavigator();
            final SWTBotTree tree = packageExplorer.bot().tree();
            tree.expandNode("MyFirstProject", true);
            tree.getTreeItem("MyFirstProject").getNode("ebin")
                    .getNode("a_module.beam").contextMenu("Delete").click();

            final SWTBotShell shell = bot.shell("Delete Resources");
            shell.activate();
            final Button button = bot.widget(widgetOfType(Button.class),
                    shell.widget);
            new SWTBotCheckBox(button).select();
            bot.button("OK").click();
            bot.waitUntil(Conditions.shellCloses(shell));

            System.out.println("deleted existing a_module_beam");
        } catch (final WidgetNotFoundException e) {
            // ignore
        }

        final SWTBotEclipseEditor textEditor = bot.activeEditor()
                .toTextEditor();
        textEditor.contextMenu("Compile file").click();
        bot.waitUntil(new DefaultCondition() {
            private SWTBotTree tree;

            @Override
            public void init(final SWTBot abot) {
                super.init(abot);
                final SWTBotView packageExplorer = getNavigator();
                tree = packageExplorer.bot().tree();
            }

            public boolean test() throws Exception {
                try {
                    tree.getTreeItem("MyFirstProject").contextMenu("Refresh")
                            .click();
                    tree.expandNode("MyFirstProject", true);
                    tree.getTreeItem("MyFirstProject").getNode("ebin")
                            .getNode("a_module.beam");
                } catch (final WidgetNotFoundException e) {
                    return false;
                }
                return true;
            }

            public String getFailureMessage() {
                return "File 'a_module.erl' wasn't compiled";
            }
        }, 5000);

    }
}
