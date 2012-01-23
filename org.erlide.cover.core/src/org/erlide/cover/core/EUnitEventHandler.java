package org.erlide.cover.core;

import org.erlide.backend.IBackend;
import org.erlide.backend.events.ErlangEventHandler;
import org.erlide.cover.views.model.TestTreeModel;
import org.erlide.cover.views.model.TestTreeObject;
import org.osgi.service.event.Event;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Handler for eunit events
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class EUnitEventHandler extends ErlangEventHandler {

    private static final String EVENT_NAME = "eunit_event";
    private static final String GROUP_BEGIN = "gbegin";
    private static final String GROUP_END = "gend";
    private static final String TEST_BEGIN = "tbegin";
    private static final String TEST_END = "tend";
    private static final String GROUP_CANCELED = "gcanceled";
    private static final String TEST_CANCELED = "tcanceled";
    private static final String SUMMARY = "result";
    private static final String SKIPPED = "skipped";
    private static final String ERROR = "error";
    private static final OtpErlangAtom MODULE_NOT_FOUND = new OtpErlangAtom(
            "module_not_found");
    private static final OtpErlangAtom NO_SUCH_FUNCTION = new OtpErlangAtom(
            "no_such_function");

    private final Logger log; // log
    private final CoverBackend coverBackend; // cover backend (if needed)
    private final TestTreeModel model;

    public EUnitEventHandler(final IBackend backend, final TestTreeModel model,
            final CoverBackend coverBackend) {
        super(EVENT_NAME, backend);
        this.coverBackend = coverBackend;
        this.model = model;
        log = Activator.getDefault();
    }

    @Override
    public void handleEvent(final Event event) {
        final OtpErlangObject data = (OtpErlangObject) event
                .getProperty("DATA");

        if (!(data instanceof OtpErlangTuple && ((OtpErlangTuple) data)
                .elementAt(0) instanceof OtpErlangAtom)) {
            return;
        }

        final OtpErlangTuple msg = (OtpErlangTuple) data;
        final OtpErlangAtom resType = (OtpErlangAtom) msg.elementAt(0);
        log.info(resType);

        if (resType.atomValue().equals(GROUP_BEGIN)) {
            handle_group_begin(msg);
        } else if (resType.atomValue().equals(TEST_END)) {
            handle_test(msg);
        } else if (resType.atomValue().equals(GROUP_END)) {
            handle_group(msg);
        } else if (resType.atomValue().equals(SUMMARY)) {
            handle_summary(msg);
            for (final IEUnitObserver obs : coverBackend.getEUnitListeners()) {
                obs.labelChanged();
            }
        } else if (resType.atomValue().equals(ERROR)) {
            handle_error(msg);
        } else if (resType.atomValue().equals(SKIPPED)) {
            handle_skipped(msg);
        } else if (resType.atomValue().equals(TEST_CANCELED)) {
            handle_test_canceled(msg);
        } else if (resType.atomValue().equals(GROUP_CANCELED)) {
            handle_group_canceled(msg);
        }

        for (final IEUnitObserver obs : coverBackend.getEUnitListeners()) {
            obs.treeChanged();
        }
    }

    // on group begining
    private void handle_group_begin(final OtpErlangTuple msg) {

        final String group = msg.elementAt(1).toString();
        final String description = msg.elementAt(2).toString();

        if (group != null && !group.equals("[]") && !group.equals("undefined")) {

            TestTreeObject node = model.findNode(description);
            TestTreeObject parent = model.findNode(group);
            if (parent == null) {
                parent = new TestTreeObject(group, TestTreeObject.WARN);
                model.addChildren(parent);
            }
            if (node != null) {
                model.removeChild(node);
            } else {
                node = new TestTreeObject(description, TestTreeObject.WARN);
            }
            parent.addChild(node);

        } else {
            final TestTreeObject node = model.findNode(description);
            if (node == null) {
                model.addChildren(new TestTreeObject(description,
                        TestTreeObject.WARN));
            }
        }
    }

    // on group cancel
    private void handle_group_canceled(final OtpErlangTuple msg) {

        final String description = msg.elementAt(1).toString();
        final String reason = msg.elementAt(2).toString();

        if (description != null && !description.equals("[]")
                && !description.equals("undefined")) {
            final TestTreeObject node = model.findNode(description);
            node.setDescription(String.format("%s ... canceled: %s",
                    node.getDescription(), reason));
        } else {
            model.addChildren(new TestTreeObject(String.format("canceled: %s",
                    reason), TestTreeObject.FAILOURE));
        }

    }

    // on test cancel
    private void handle_test_canceled(final OtpErlangTuple msg) {

        final String group = msg.elementAt(1).toString();
        final String description = msg.elementAt(2).toString();
        final int line = Integer.parseInt(msg.elementAt(3).toString());
        final String reason = msg.elementAt(4).toString();

        final OtpErlangTuple source = (OtpErlangTuple) msg.elementAt(5);
        final String module = source.elementAt(0).toString();
        final String function = source.elementAt(1).toString();
        final int arity = Integer.parseInt(source.elementAt(2).toString());

        final TestTreeObject node = new TestTreeObject(
                makeTestShortDescription(module, function, arity),
                TestTreeObject.FAILOURE);

        node.setDescription(makeTestFullDescription(module, function, arity,
                description, line, String.format("canceled: %s", reason)));

        model.findNode(group).addChild(node);

    }

    // on skipped test
    private void handle_skipped(final OtpErlangTuple msg) {

        final String group = msg.elementAt(2).toString();
        final String description = msg.elementAt(3).toString();
        final int line = Integer.parseInt(msg.elementAt(4).toString());

        final OtpErlangTuple source = (OtpErlangTuple) msg.elementAt(5);
        final String module = source.elementAt(0).toString();
        final String function = source.elementAt(1).toString();
        final int arity = Integer.parseInt(source.elementAt(2).toString());

        String reason = "";

        if (msg.elementAt(1).equals(MODULE_NOT_FOUND)) {

            reason = String.format("Module not found: %s", msg.elementAt(5));

        } else if (msg.elementAt(1).equals(NO_SUCH_FUNCTION)) {

            final OtpErlangTuple func = (OtpErlangTuple) msg.elementAt(5);

            reason = String.format("No such function: %s:%s/%s", func
                    .elementAt(1).toString(), func.elementAt(2).toString(),
                    func.elementAt(3).toString());
        }

        final TestTreeObject node = new TestTreeObject(
                makeTestShortDescription(module, function, arity),
                TestTreeObject.FAILOURE);

        node.setDescription(makeTestFullDescription(module, function, arity,
                description, line, String.format("skipped: %s", reason)));

        model.findNode(group).addChild(node);

    }

    // on failed test
    private void handle_error(final OtpErlangTuple msg) {

        final String group = msg.elementAt(1).toString();
        final String description = msg.elementAt(2).toString();
        final int line = Integer.parseInt(msg.elementAt(3).toString());

        final OtpErlangTuple source = (OtpErlangTuple) msg.elementAt(4);
        final String module = source.elementAt(0).toString();
        final String function = source.elementAt(1).toString();
        final int arity = Integer.parseInt(source.elementAt(2).toString());

        final String exception = msg.elementAt(5).toString();

        final TestTreeObject node = new TestTreeObject(
                makeTestShortDescription(module, function, arity),
                TestTreeObject.FAILOURE);

        node.setDescription(makeTestFullDescription(module, function, arity,
                description, line, "error"));
        node.addChild(new TestTreeObject(exception, TestTreeObject.DESCR));

        model.findNode(group).addChild(node);

    }

    // at the end of testing
    private void handle_summary(final OtpErlangTuple msg) {

        model.updatePass(Integer.parseInt(msg.elementAt(1).toString()));
        model.updateFail(Integer.parseInt(msg.elementAt(2).toString()));
        model.updateSkip(Integer.parseInt(msg.elementAt(3).toString()));
        model.updateCancel(Integer.parseInt(msg.elementAt(4).toString()));

    }

    // on group ending
    private void handle_group(final OtpErlangTuple msg) {

        final String description = msg.elementAt(1).toString();
        final int time = Integer.parseInt(msg.elementAt(2).toString());

        final TestTreeObject node = model.findNode(description);
        node.setTime(time);
        node.updateType();

    }

    // on test success
    private void handle_test(final OtpErlangTuple msg) {

        final String group = msg.elementAt(1).toString();
        final String description = msg.elementAt(2).toString();
        final int line = Integer.parseInt(msg.elementAt(3).toString());
        final int time = Integer.parseInt(msg.elementAt(5).toString());

        final OtpErlangTuple source = (OtpErlangTuple) msg.elementAt(4);
        final String module = source.elementAt(0).toString();
        final String function = source.elementAt(1).toString();
        final int arity = Integer.parseInt(source.elementAt(2).toString());

        final TestTreeObject node = new TestTreeObject(
                makeTestShortDescription(module, function, arity),
                TestTreeObject.SUCCESS);

        node.setDescription(makeTestFullDescription(module, function, arity,
                description, line, "ok"));
        node.setTime(time);

        model.findNode(group).addChild(node);

    }

    // test name
    private String makeTestShortDescription(final String module,
            final String function, final int arity) {
        return String.format("%s:%s/%d", module, function, arity);
    }

    // test full description
    private String makeTestFullDescription(final String module,
            final String function, final int arity, final String description,
            final int line, final String status) {
        if (description != null && !description.equals("undefined")) {
            return String.format("%s:%s/%d (%s) at line %d - %s", module,
                    function, arity, description, line, status);
        } else {
            return String.format("%s:%s/%d at line %d - %s", module, function,
                    arity, line, status);
        }
    }

}
