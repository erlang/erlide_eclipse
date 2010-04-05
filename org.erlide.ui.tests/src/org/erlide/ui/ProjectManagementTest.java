package org.erlide.ui;

import junit.framework.Assert;

import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.junit.SWTBotJunit4ClassRunner;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(SWTBotJunit4ClassRunner.class)
public class ProjectManagementTest {

	private static SWTWorkbenchBot bot;

	@BeforeClass
	public static void beforeClass() throws Exception {
		bot = new SWTWorkbenchBot();
		bot.viewByTitle("Welcome").close();
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
		Assert.assertTrue(isProjectCreated("MyFirstProject"));

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
		bot.waitUntil(org.eclipse.swtbot.swt.finder.waits.Conditions
				.shellCloses(ashell));
	}

	@AfterClass
	public static void sleep() {
		bot.sleep(2000);
	}

	private static SWTBotView getNavigator() {
		SWTBotView view = bot.viewByTitle("Erlang Navigator");
		return view;
	}

	private boolean isProjectCreated(final String name) {
		try {
			SWTBotView packageExplorer = getNavigator();
			SWTBotTree tree = packageExplorer.bot().tree();
			tree.getTreeItem(name);
			return true;
		} catch (WidgetNotFoundException e) {
			return false;
		}
	}

	private void waitForShellToDisappear(final String title) {
		try {
			while (bot.shell(title).isActive()) {
				// wait
			}
		} catch (WidgetNotFoundException e) {
			// Ignore
		}
	}

}
