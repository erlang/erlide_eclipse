package org.erlide.ui;

import static org.junit.Assert.fail;
import junit.framework.Assert;

import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.waits.Conditions;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEclipseEditor;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.waits.DefaultCondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotRadio;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(SWTBotJunit4ClassRunner.class)
public class ProjectManagementTest {

	private static SWTWorkbenchBot bot;
	private static SWTBotShell mainshell;

	@BeforeClass
	public static void setupEnvironment() {
		bot = new SWTWorkbenchBot();
		bot.viewByTitle("Welcome").close();
		bot.perspectiveByLabel("Erlang").activate();
		// bot.waitUntil(Conditions.waitForMenu(bot.activeShell(), ));
		SWTBotMenu autoBuildMenu = bot.menu("Project", 1).menu(
				"Build Automatically");
		if (autoBuildMenu.isChecked()) {
			autoBuildMenu.click();
		}
		mainshell = bot.activeShell();
	}

	@AfterClass
	public static void cleanupEnvironment() {
		SWTBotView packageExplorer = getNavigator();
		SWTBotTree tree = packageExplorer.bot().tree();
		packageExplorer.show();
		tree.select("MyFirstProject");
		bot.menu("Edit").menu("Delete").click();
		SWTBotShell ashell = bot.shell("Delete Resources");
		ashell.activate();
		bot.checkBox("Delete project contents on disk (cannot be undone)")
				.select();
		bot.button("OK").click();
		bot.waitUntil(Conditions.shellCloses(ashell));
	}

	@Test
	public void canCreateANewErlangProject() throws Exception {
		bot.menu("File").menu("New").menu("Project...").click();

		SWTBotShell shell = bot.shell("New Project");
		shell.activate();
		bot.tree().expandNode("Erlang").select("Erlang Project");
		bot.button("Next >").click();

		bot.textWithLabel("Project name:").setText("MyFirstProject");

		bot.button("Finish").click();
		// FIXME: assert that the project is actually created, for later
		Assert.assertTrue("Project could not be created",
				isProjectCreated("MyFirstProject"));

	}

	private static SWTBotView getNavigator() {
		SWTBotView view = bot.viewByTitle("Erlang Navigator");
		return view;
	}

	private static boolean isProjectCreated(final String name) {
		try {
			SWTBotView packageExplorer = getNavigator();
			SWTBotTree tree = packageExplorer.bot().tree();
			tree.getTreeItem(name);
			return true;
		} catch (WidgetNotFoundException e) {
			return false;
		}
	}

	private static void waitForShellToDisappear(final String title) {
		try {
			while (bot.shell(title).isActive()) {
				bot.sleep(1);
			}
		} catch (WidgetNotFoundException e) {
			// Ignore
		}
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
			SWTBotView packageExplorer = getNavigator();
			SWTBotTree tree = packageExplorer.bot().tree();
			tree.expandNode("MyFirstProject", true);
			tree.getTreeItem("MyFirstProject").getNode("src").getNode(
					"a_module.erl").doubleClick();
			// bot.activeEditor().toTextEditor().setText("-module(a_module).\n");
			bot.activeEditor().save();
		} catch (WidgetNotFoundException e) {
			fail("Module 'a_module.erl' has NOT been created: "
					+ e.getMessage());
		}
	}

	// @Test
	public void canCompileModule() {
		deleteBeamFile();

		SWTBotEclipseEditor textEditor = bot.activeEditor().toTextEditor();
		textEditor.contextMenu("Compile file").click();
		bot.waitUntil(new DefaultCondition() {
			public boolean test() throws Exception {
				try {
					SWTBotView packageExplorer = getNavigator();
					SWTBotTree tree = packageExplorer.bot().tree();
					tree.getTreeItem("MyFirstProject").contextMenu("Refresh")
							.click();
					tree.expandNode("MyFirstProject", true);
					tree.getTreeItem("MyFirstProject").getNode("ebin").getNode(
							"a_module.beam");
				} catch (WidgetNotFoundException e) {
					return false;
				}
				return true;
			}

			public String getFailureMessage() {
				return "File 'a_module.erl' wasn't compiled";
			}
		}, 5000);
	}

	private void deleteBeamFile() {
		try {
			SWTBotView packageExplorer = getNavigator();
			SWTBotTree tree = packageExplorer.bot().tree();
			tree.expandNode("MyFirstProject", true);
			tree.getTreeItem("MyFirstProject").getNode("ebin").getNode(
					"a_module.beam").contextMenu("Delete").click();
			System.out.println("deleted existing a_module_beam");
		} catch (WidgetNotFoundException e) {
			// ignore
		}
	}

	//@Test
	public void canBuildProject() {
		deleteBeamFile();
		// SWTBotMenu menu = bot.menu("Project", 1);
		// SWTBotMenu autoBuildMenu = menu.menu("Build Automatically");
		// if (autoBuildMenu.isChecked()) {
		// autoBuildMenu.click();
		// }
		try {
			final SWTBotMenu pmenu = bot.menu("Project", 1);
			SWTBotMenu cmenu = pmenu.menu("Clean...");
			cmenu.click();
			SWTBotShell ashell = bot.shell("Clean");
			ashell.activate();
			SWTBotRadio cb = bot.radio("Clean all projects");
			if (!cb.isSelected()) {
				cb.click();
			}
			bot.button("OK").click();
			bot.waitUntil(Conditions.shellCloses(ashell));
			bot.waitUntil(new DefaultCondition() {

				public String getFailureMessage() {
					return "Project was not rebuilt";
				}

				public boolean test() throws Exception {
					try {
						SWTBotView packageExplorer = getNavigator();
						SWTBotTree tree = packageExplorer.bot().tree();
						tree.getTreeItem("MyFirstProject").contextMenu(
								"Refresh").click();
						tree.expandNode("MyFirstProject", true);
						tree.getTreeItem("MyFirstProject").getNode("ebin")
								.getNode("a_module.beam");
					} catch (WidgetNotFoundException e) {
						return false;
					}
					return true;
				}
			}, 5000);
		} finally {
			// autoBuildMenu.click();
		}
	}
}
