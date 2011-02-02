package org.erlide.cover.core;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.erlide.cover.views.model.FunctionStats;
import org.erlide.cover.views.model.ICoverageStats;
import org.erlide.cover.views.model.IStatsTreeObject;
import org.erlide.cover.views.model.LineResult;
import org.erlide.cover.views.model.ModuleSet;
import org.erlide.cover.views.model.ModuleStats;
import org.erlide.cover.views.model.StatsTreeModel;
import org.erlide.jinterface.backend.events.ErlangEvent;
import org.erlide.jinterface.backend.events.EventHandler;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Handler for coverage events
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverEventHandler extends EventHandler {

    private static final String EVENT_NAME = "cover_event";
    private static final String COVER_FIN = "cover_fin";
    private static final String COVER_ERROR = "cover_error";
    private static final String COVER_RES = "module_res";
    private static final String INDEX = "index";

    private final List<ICoverObserver> listeners = new LinkedList<ICoverObserver>();
    private int moduleNum;
    private int counter;
    private ICoverAnnotationMarker annotationMarker;

    public void addListener(final ICoverObserver listener) {
        System.out.println("adding listener");
        listeners.add(listener);
    }

    public List<ICoverObserver> getListeners() {
        return listeners;
    }

    public void addAnnotationMaker(final ICoverAnnotationMarker am) {
        annotationMarker = am;
    }

    public ICoverAnnotationMarker getAnnotationMaker() {
        return annotationMarker;
    }

    @Override
    protected void doHandleEvent(final ErlangEvent event) throws Exception {
        if (!event.hasTopic(EVENT_NAME)) {
            return;
        }

        OtpErlangTuple tuple = null;

        if (gotResults(event.data)) {
            for (final ICoverObserver obs : listeners) {
                obs.eventOccured(new CoverEvent(CoverStatus.UPDATE));
            }
        } else if ((tuple = getErrorReason(event.data)) != null) {
            final String place = tuple.elementAt(1).toString();
            final String type = tuple.elementAt(2).toString();
            final String info = tuple.elementAt(3).toString();

            for (final ICoverObserver obs : listeners) {
                obs.eventOccured(new CoverEvent(CoverStatus.ERROR,
                        String.format("Error at %s while %s: %s\n", place,
                                type, info)));
            }
        } else if (event.toString().equals(COVER_FIN)) {
            getAnnotationMaker().addAnnotations();
        }

    }

    /**
     * @deprecated
     * @param msg
     * @return
     */
    @Deprecated
    private boolean gotIndex(final OtpErlangObject msg) {
        if (msg instanceof OtpErlangTuple) {
            final OtpErlangTuple resTuple = (OtpErlangTuple) msg;
            if (resTuple.elementAt(0) instanceof OtpErlangAtom
                    && resTuple.elementAt(0).toString().equals(INDEX)) {

                final String htmlPath = resTuple.elementAt(1).toString();
                StatsTreeModel.getInstance()
                        .setIndex(
                                htmlPath.substring(1, htmlPath.toString()
                                        .length() - 1));
            }
        }
        return false;
    }

    /**
     * When coverage results came
     * 
     * @param msg
     * @return
     */
    private boolean gotResults(final OtpErlangObject msg) {
        if (msg instanceof OtpErlangTuple) {
            final OtpErlangTuple resTuple = (OtpErlangTuple) msg;
            if (resTuple.elementAt(0) instanceof OtpErlangAtom
                    && ((OtpErlangAtom) resTuple.elementAt(0)).atomValue()
                            .equals(COVER_RES)) {

                final StatsTreeModel model = StatsTreeModel.getInstance();
                final IStatsTreeObject root = model.getRoot();

                final String moduleName = resTuple.elementAt(1).toString();

                String htmlPath = resTuple.elementAt(2).toString();
                htmlPath = htmlPath.substring(1, htmlPath.length() - 1);
                int allLines = Integer.parseInt(resTuple.elementAt(3)
                        .toString());
                int coveredLines = Integer.parseInt(resTuple.elementAt(4)
                        .toString());
                final double percent = Double.parseDouble(resTuple.elementAt(5)
                        .toString());

                System.out.format("Module %s %s %d %d %f", moduleName,
                        htmlPath, allLines, coveredLines, percent);

                final ModuleStats moduleStats = new ModuleStats();

                moduleStats.setLabel(moduleName);
                moduleStats.setHtmlPath(htmlPath);
                moduleStats.setLiniesCount(allLines);
                moduleStats.setCoverCount(coveredLines);
                moduleStats.setPercentage(percent);

                prepLineResults((OtpErlangList) resTuple.elementAt(6),
                        moduleStats);

                prepFuncResults((OtpErlangList) resTuple.elementAt(7),
                        moduleStats);

                final ICoverageStats moduleOld = root.findChild(moduleStats
                        .getLabel());
                if (moduleOld != null) {
                    allLines -= moduleOld.getLinesCount();
                    coveredLines -= moduleOld.getCoverCount();
                }
                model.addTotal(allLines, coveredLines);

                root.addChild(moduleStats.getLabel(), moduleStats);

                ModuleSet.add(moduleStats);

                return true;
            }
            return false;
        }
        return false;
    }

    private void prepFuncResults(final OtpErlangList funcList,
            final ModuleStats stats) {

        final Iterator<OtpErlangObject> it = funcList.iterator();

        while (it.hasNext()) {
            final OtpErlangTuple res = (OtpErlangTuple) it.next();

            final FunctionStats func = new FunctionStats();

            final String name = res.elementAt(1).toString();
            final int arity = Integer.parseInt(res.elementAt(2).toString());
            final int allLines = Integer.parseInt(res.elementAt(3).toString());
            final int coveredLines = Integer.parseInt(res.elementAt(4)
                    .toString());
            final double percent = Double.parseDouble(res.elementAt(5)
                    .toString());

            func.setLabel(name);
            func.setLiniesCount(allLines);
            func.setCoverCount(coveredLines);
            func.setPercentage(percent);
            func.setArity(arity);

            stats.addChild(func.getLabel(), func);
        }

    }

    private void prepLineResults(final OtpErlangList lineList,
            final ModuleStats stats) {

        final Iterator<OtpErlangObject> it = lineList.iterator();

        while (it.hasNext()) {
            final OtpErlangTuple res = (OtpErlangTuple) it.next();
            final int num = Integer.parseInt(res.elementAt(1).toString());
            final int calls = Integer.parseInt(res.elementAt(2).toString());
            final LineResult lineRes = new LineResult(num, calls);

            stats.addLine(lineRes);

        }

    }

    private OtpErlangTuple getErrorReason(final OtpErlangObject message) {
        if (message instanceof OtpErlangTuple) {
            final OtpErlangTuple tuple = (OtpErlangTuple) message;
            if (tuple.elementAt(0) instanceof OtpErlangAtom
                    && ((OtpErlangAtom) tuple.elementAt(0)).atomValue().equals(
                            COVER_ERROR)) {

                return tuple;
            }
        }
        return null;
    }

}
