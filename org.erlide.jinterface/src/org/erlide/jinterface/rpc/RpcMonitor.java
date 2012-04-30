package org.erlide.jinterface.rpc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpNode;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class RpcMonitor {
    private static final boolean DISABLED = System
            .getProperty("erlide.rpcmonitor") == null;
    private static final int COUNT = Integer.parseInt(System.getProperty(
            "erlide.rpcmonitor.count", "50"));
    private static final boolean FULL = System
            .getProperty("erlide.rpcmonitor.full") != null;

    private static class RpcData {
        public final long startTime;
        public final String node;
        public final String module;
        public final String fun;
        public final SoftReference<Collection<OtpErlangObject>> args;
        public final long size;

        public RpcData(final long startTime, final String node,
                final String module, final String fun,
                final OtpErlangObject[] args, final long size) {
            this.startTime = startTime;
            this.node = node;
            this.module = module;
            this.fun = fun;
            this.args = new SoftReference<Collection<OtpErlangObject>>(
                    Collections.unmodifiableCollection(Lists.newArrayList(args)));
            this.size = size;
        }
    }

    private static class RpcInfo {
        public final String node;
        public final String module;
        public final String fun;
        public final SoftReference<Collection<OtpErlangObject>> args;
        public final long callSize;
        public final SoftReference<OtpErlangObject> result;
        public final long answerSize;
        public final long callTime;
        public final long answerTime;
        private final int argsSize;

        public RpcInfo(final RpcData data, final OtpErlangObject result,
                final long answerTime) {
            node = data.node;
            module = data.module;
            fun = data.fun;
            args = data.args;
            argsSize = args.get().size();
            this.result = new SoftReference<OtpErlangObject>(result);
            callTime = data.startTime;
            this.answerTime = answerTime;
            callSize = data.size;
            answerSize = OtpErlang.sizeOf(result);
        }

        public void dump(final PrintStream out, final boolean full) {
            Collection<OtpErlangObject> myArgs = args.get();
            myArgs = myArgs == null ? new ArrayList<OtpErlangObject>() : myArgs;
            final String argsString = full ? args.toString().replaceAll(
                    "\n|\r", " ") : "...";
            OtpErlangObject val = result.get();
            val = val == null ? new OtpErlangAtom("null") : val;
            String resultString = full ? val.toString()
                    .replaceAll("\n|\r", " ") : "...";
            if (resultString.length() > 100) {
                resultString = new String(resultString.substring(0, 99) + "...");
            }
            out.format(
                    "%30s|%25s:%-20s/%d in=%9d, out=%9d, t=%6d, args=%s -> result=%s%n",
                    node.substring(0, Math.min(29, node.length() - 1)), module,
                    fun, argsSize, callSize, answerSize, answerTime - callTime,
                    argsString, resultString);
        }
    }

    private static int callCount = 0;
    private static final Map<OtpErlangRef, RpcData> ongoing = Maps.newHashMap();
    private static Comparator<RpcInfo> timeComparator = new Comparator<RpcInfo>() {
        @Override
        public int compare(final RpcInfo o1, final RpcInfo o2) {
            return (int) (o2.answerTime - o2.callTime - (o1.answerTime - o1.callTime));
        }
    };
    private static Comparator<RpcInfo> sizeComparator = new Comparator<RpcInfo>() {
        @Override
        public int compare(final RpcInfo o1, final RpcInfo o2) {
            return (int) (o2.callSize + o2.answerSize - (o1.callSize + o1.answerSize));
        }
    };
    private static final List<RpcInfo> slowest = Lists.newLinkedList();
    private static final List<RpcInfo> largest = Lists.newLinkedList();

    public synchronized static void recordResponse(final OtpErlangRef ref,
            final OtpErlangObject result) {
        if (DISABLED) {
            return;
        }
        final RpcData data = ongoing.remove(ref);
        final long now = System.currentTimeMillis();
        final RpcInfo info = new RpcInfo(data, result, now);
        add(largest, sizeComparator, info);
        add(slowest, timeComparator, info);
    }

    private static void add(final List<RpcInfo> list,
            final Comparator<RpcInfo> comparator, final RpcInfo info) {
        final int index = Collections.binarySearch(list, info, comparator);
        if (index < 0) {
            // Add the non-existent item to the list
            list.add(-index - 1, info);
        } else {
            list.add(index, info);
        }
        if (list.size() > COUNT) {
            list.remove(list.size() - 1);
        }
    }

    public static OtpErlangRef recordRequest(final OtpNode node,
            final String peer, final String module, final String fun,
            final OtpErlangObject[] args, final long callSize) {
        callCount++;
        if (DISABLED) {
            return null;
        }
        final RpcData data = new RpcData(System.currentTimeMillis(), peer,
                module, fun, args, callSize);
        final OtpErlangRef ref = node.createRef();
        ongoing.put(ref, data);
        return ref;
    }

    public static void dump() {
        dump(System.out, COUNT, FULL);
    }

    public static void dump(final String file) {
        dump(file, COUNT, FULL);
    }

    public static void dump(final String file, final int n) {
        dump(file, n, FULL);
    }

    public static void dump(final String file, final boolean full) {
        dump(file, COUNT, full);
    }

    public static void dump(final String fileName, final int n,
            final boolean full) {
        try {
            dump(new PrintStream(fileName), n, full);
        } catch (final FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public synchronized static void dump(final PrintStream out, final int n,
            final boolean full) {
        out.format("*** RpcMonitor statistics%n - %d calls%n", callCount);
        if (DISABLED) {
            out.println("\nRpcMonitor was not enabled.\n\nUse -Derlide.rpcmonitor to enable it.");
            return;
        }
        final String delim = "----------------------------------------------------------------------------------------";
        out.println(delim);
        out.println();
        out.format("Slowest %d calls%n", slowest.size());
        out.println(delim);
        for (final RpcInfo info : slowest) {
            info.dump(out, full);
        }
        out.println(delim);
        out.println();
        out.format("Largest %d calls%n", largest.size());
        out.println(delim);
        for (final RpcInfo info : largest) {
            if (info != null) {
                info.dump(out, full);
            }
        }
        out.println(delim);
        out.close();
    }

    public static void cleanupOldLogs(final String dirName, final String prefix) {
        final File dir = new File(dirName);
        for (final File f : dir.listFiles()) {
            final long now = System.currentTimeMillis();
            final int aWeek = 7 * 24 * 3600 * 1000;
            if (f.getName().startsWith(prefix)
                    && now - f.lastModified() > aWeek) {
                f.delete();
            }
        }
    }

}
