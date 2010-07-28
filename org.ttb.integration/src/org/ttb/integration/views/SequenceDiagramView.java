package org.ttb.integration.views;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.ui.part.ViewPart;
import org.ttb.integration.TtbBackend;
import org.ttb.integration.mvc.model.ITraceNodeObserver;
import org.ttb.integration.mvc.model.TracePattern;

/**
 * Sequence diagram which shows tracing results.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class SequenceDiagramView extends ViewPart implements ITraceNodeObserver {

    private final String CONSOLE_NAME = "ttb console";

    public SequenceDiagramView() {
        TtbBackend.getInstance().addListener(this);

        // TODO delete
        MessageConsole console = findConsole(CONSOLE_NAME);
        MessageConsoleStream out = console.newMessageStream();
        out.println("Hello from Generic console sample action");
        out.println("starting");
        out.println("ok");
    }

    private MessageConsole findConsole(String name) {
        ConsolePlugin plugin = ConsolePlugin.getDefault();
        IConsoleManager conMan = plugin.getConsoleManager();
        IConsole[] existing = conMan.getConsoles();
        for (int i = 0; i < existing.length; i++)
            if (name.equals(existing[i].getName()))
                return (MessageConsole) existing[i];
        // no console found, so create a new one
        MessageConsole myConsole = new MessageConsole(name, null);
        conMan.addConsoles(new IConsole[] { myConsole });
        return myConsole;
    }

    @Override
    public void dispose() {
        TtbBackend.getInstance().removeListener(this);
        super.dispose();
    }

    @Override
    public void createPartControl(Composite parent) {
        // TODO
    }

    @Override
    public void setFocus() {
        // TODO Auto-generated method stub

    }

    @Override
    public void addPattern(TracePattern tracePattern) {
        // TODO
    }

    @Override
    public void removePattern(TracePattern tracePattern) {
        // TODO
    }

    @Override
    public void updatePattern(TracePattern tracePattern) {
        // TODO Auto-generated method stub
    }

    @Override
    public void startTracing() {
        // TODO Auto-generated method stub
    }

    @Override
    public void stopTracing() {
        // TODO Auto-generated method stub

    }
}
