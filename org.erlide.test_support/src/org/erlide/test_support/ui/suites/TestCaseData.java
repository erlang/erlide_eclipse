package org.erlide.test_support.ui.suites;

import java.util.Collection;
import java.util.List;

import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.util.ErlUtils;
import org.erlide.jinterface.util.TermParserException;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class TestCaseData {

    enum TestState {
        // order is important!
        NOT_RUN, SUCCESS, SKIPPED, RUNNING, FAILED
    }

    public class FailLocations {

        private final Collection<FailLocation> locations;

        public FailLocations(final Collection<OtpErlangObject> locs) {
            locations = Lists.newArrayList();
            for (final OtpErlangObject item : locs) {
                locations.add(new FailLocation(item));
            }
            // first element points to ourselves, ignore it
            locations.remove(locations.iterator().next());
        }

        public Collection<FailLocation> getLocations() {
            return locations;
        }

        public boolean isEmpty() {
            return locations.isEmpty();
        }

    }

    public class FailLocation {

        private final OtpErlangObject location;

        public FailLocation(final OtpErlangObject location) {
            this.location = location;
        }

        @Override
        public String toString() {
            return location.toString();
        }

    }

    public class FailReason {
        private final Collection<FailStackItem> items;
        private final String reason;

        public FailReason(final String reason,
                final Collection<OtpErlangObject> stack) {
            this.reason = reason;
            items = Lists.newArrayList();
            for (final OtpErlangObject item : stack) {
                items.add(new FailStackItem(item));
            }
        }

        public Collection<FailStackItem> getStackItems() {
            return items;
        }

        public String getReason() {
            return reason;
        }

        public FailStackItem getFirstStackItem() {
            // assume there's always some item
            return items.iterator().next();
        }
    }

    public class FailStackItem {

        private final OtpErlangObject item;
        private String m;
        private String f;
        private OtpErlangObject a;

        public FailStackItem(final OtpErlangObject item) {
            this.item = item;
        }

        @Override
        public String toString() {
            try {
                final Bindings b = ErlUtils.match("{M:a, F:a, A}", item);
                m = b.getQuotedAtom("M");
                f = b.getQuotedAtom("F");
                a = b.get("A");
                final String aa = a.toString();
                final String args = a instanceof OtpErlangLong ? " / "
                        + a.toString() : " ( "
                        + aa.substring(1, aa.length() - 2) + " )";
                return m + " : " + f + args;
            } catch (final TermParserException e) {
                ErlLogger.warn(e);
            } catch (final OtpErlangException e) {
                ErlLogger.warn(e);
            }
            return item.toString();
        }

        public String getModule() {
            return m;
        }

        public String getFunction() {
            return f;
        }

    }

    private final String suite;
    private final String testcase;
    private TestState state;
    private FailReason failStack;
    private FailLocations failLocations;
    private OtpErlangObject skipComment;

    public TestCaseData(final String mod, final String fun) {
        suite = mod;
        testcase = fun;
        state = TestState.NOT_RUN;
    }

    @Override
    public String toString() {
        return suite + ":" + testcase;
    }

    public void setRunning() {
        state = TestState.RUNNING;
    }

    public void setSuccesful() {
        state = TestState.SUCCESS;
    }

    public void setFailed(final OtpErlangObject reason,
            final Collection<OtpErlangObject> locations) {
        state = TestState.FAILED;
        failStack = parseReason(reason);
        failLocations = new FailLocations(locations);
    }

    private Collection<FailLocation> parseLocations(
            final Collection<OtpErlangObject> locations) {
        final List<FailLocation> result = Lists.newArrayList();
        for (final OtpErlangObject location : locations) {
            result.add(new FailLocation(location));
        }
        return result;
    }

    private FailReason parseReason(final OtpErlangObject reason) {
        Bindings b;
        try {
            b = ErlUtils.match("{Cause:a, Stack}", reason);
            final Collection<OtpErlangObject> stack = b.getList("Stack");
            return new FailReason(b.getAtom("Cause"), stack);
        } catch (final TermParserException e) {
            ErlLogger.warn(e);
        } catch (final OtpErlangException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    public String getModule() {
        return suite;
    }

    public String getFunction() {
        return testcase;
    }

    public TestState getState() {
        return state;
    }

    public FailLocations getFailLocations() {
        return failLocations;
    }

    public FailReason getFailStack() {
        return failStack;
    }

    public void setSkipped(final OtpErlangObject comment) {
        state = TestState.SKIPPED;
        skipComment = comment;
    }

    public OtpErlangObject getSkipComment() {
        return skipComment;
    }
}
