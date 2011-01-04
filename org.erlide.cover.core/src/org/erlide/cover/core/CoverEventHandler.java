package org.erlide.cover.core;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.erlide.cover.views.model.FunctionStats;
import org.erlide.cover.views.model.IStatsTreeObject;
import org.erlide.cover.views.model.LineResult;
import org.erlide.cover.views.model.ModuleStats;
import org.erlide.cover.views.model.StatsTreeModel;
import org.erlide.jinterface.backend.events.EventHandler;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Handler for cover events
 * 
 * @author Aleksandra Lipiec
 * 
 */
public class CoverEventHandler extends EventHandler {

    private static final String EVENT_NAME = "cover_event";

    private static final String COVER_OK = "cover_ok";

    private static final String COVER_ERROR = "cover_error";

    private static final String COVER_RES = "module_res";

    private static final String TESTING_FINISHED = "tst_finish";

    private List<ICoverObserver> listeners = new LinkedList<ICoverObserver>();

    public void addListener(ICoverObserver listener) {
        System.out.println("adding listener");
        listeners.add(listener);
    }

    public List<ICoverObserver> getListeners() {
        return listeners;
    }

    @Override
    protected void doHandleMsg(OtpErlangObject msg) throws Exception {
        OtpErlangObject event = getStandardEvent(msg, EVENT_NAME);
        OtpErlangObject errorReason = null;

        System.out.println("Received mesg " + msg);

        if (event == null)
            return;

        System.out.println(event.toString());

        if (gotResults(event)) {
            for (ICoverObserver obs : listeners)
                obs.updateViewer();
            System.out.println("Got results!");
        } else if ((errorReason = getErrorReason(event)) != null) {

        }

    }

    /**
     * When coverage results came
     * 
     * @param msg
     * @return
     */
    private boolean gotResults(OtpErlangObject msg) {
        if (msg instanceof OtpErlangTuple) {
            OtpErlangTuple resTuple = (OtpErlangTuple) msg;
            if (resTuple.elementAt(0) instanceof OtpErlangAtom
                    && ((OtpErlangAtom) resTuple.elementAt(0)).atomValue()
                            .equals(COVER_RES)) {

                System.out.println("Results: " + resTuple);
                
                StatsTreeModel model = StatsTreeModel.getInstance();
                IStatsTreeObject root = model.getRoot();
                
                String moduleName = resTuple.elementAt(1).toString();
                
                String htmlPath = resTuple.elementAt(2).toString();
                int allLines = Integer.parseInt(
                        resTuple.elementAt(3).toString());
                int coveredLines = Integer.parseInt(
                        resTuple.elementAt(4).toString());
                double percent = Double.parseDouble(
                        resTuple.elementAt(5).toString());
                
                System.out.format("Module %s %s %d %d %f", moduleName,
                        htmlPath, allLines, coveredLines, percent);
                
                ModuleStats moduleStats = new ModuleStats();
                
                moduleStats.setLabel(moduleName);
                moduleStats.setHtmlPath(htmlPath);
                moduleStats.setLiniesCount(allLines);
                moduleStats.setCoverCount(coveredLines);
                moduleStats.setPercentage(percent);
                
                prepLineResults((OtpErlangList)resTuple.elementAt(6),
                        moduleStats);
                
                prepFuncResults((OtpErlangList)resTuple.elementAt(7),
                        moduleStats);
                
                model.addTotal(allLines, coveredLines);
                root.addChild(moduleStats);
                
                return true;
            }
            return false;
        }
        return false;
    }

    private void prepFuncResults(OtpErlangList funcList,
            ModuleStats stats) {
        
        Iterator<OtpErlangObject> it = funcList.iterator();
        
        while(it.hasNext()) {
            OtpErlangTuple res = (OtpErlangTuple)it.next();
            
            FunctionStats func = new FunctionStats();
            
            String name = res.elementAt(1).toString();
            int arity = Integer.parseInt(res.elementAt(2).toString());
            int allLines = Integer.parseInt(res.elementAt(3).toString());
            int coveredLines = Integer.parseInt(res.elementAt(4).toString());
            double percent = Double.parseDouble(res.elementAt(5).toString());
            
            func.setLabel(name);
            func.setLiniesCount(allLines);
            func.setCoverCount(coveredLines);
            func.setPercentage(percent);
            func.setArity(arity);
            
            System.out.println(func);
            
            stats.addChild(func);
        }
        
    }

    private void prepLineResults(OtpErlangList lineList,
            ModuleStats stats) {
        
        Iterator<OtpErlangObject> it = lineList.iterator();
        
        while(it.hasNext()) {
            OtpErlangTuple res = (OtpErlangTuple)it.next();
            int num = Integer.parseInt(res.elementAt(1).toString());
            int calls = Integer.parseInt(res.elementAt(2).toString());
            LineResult lineRes = new LineResult(num, calls);
            System.out.println(lineRes);
            
            stats.addLine(lineRes);
            
        }
        
    }

    // remove
  /*  private boolean isCoveringFinished(OtpErlangObject msg) {
        if (msg instanceof OtpErlangTuple) {
            OtpErlangTuple mesgTuple = (OtpErlangTuple) msg;
            if (mesgTuple.elementAt(0) instanceof OtpErlangAtom
                    && ((OtpErlangAtom) mesgTuple.elementAt(0)).atomValue()
                            .equals(COVER_OK)) {

                ErlLogger.debug("Mesg ok");

                System.out.println(mesgTuple.toString());

                OtpErlangTuple res = (OtpErlangTuple) mesgTuple.elementAt(1);
                System.out.println("0:: " + res);

              //  changeStatModel(res);

                return true;
            }
        }
        return false;
    }*/

    private OtpErlangObject getErrorReason(OtpErlangObject message) {
        if (message instanceof OtpErlangTuple) {
            OtpErlangTuple tuple = (OtpErlangTuple) message;
            if (tuple.elementAt(0) instanceof OtpErlangAtom
                    && ((OtpErlangAtom) tuple.elementAt(0)).atomValue().equals(
                            COVER_ERROR)) {
                
                String place = tuple.elementAt(1).toString();
                String type = tuple.elementAt(2).toString();
                String info = tuple.elementAt(3).toString();
                
                

                ErlLogger.debug("Mesg error");
                
                for (ICoverObserver obs : listeners)
                    obs.showError(place, type, info);

                return tuple.elementAt(1);
            }
        }
        return null;
    }

    /**
     * appends new results to statistics tree
     * 
     * @param res
     */
   /* private void changeStatModel(OtpErlangTuple res) {

        // TODO: append should be possible on some conditions

        StatsTreeModel model = StatsTreeModel.getInstance();
        // TODO: should be changed at some point probably
        IStatsTreeObject root = model.getRoot();

        try {
            System.out.println("1: " + res.elementAt(0));
            System.out.println("1: "
                    + ((OtpErlangTuple) res.elementAt(0)).elementAt(1));
            OtpErlangObject tot = ((OtpErlangTuple) res.elementAt(0))
                    .elementAt(1);
            System.out.println("1: " + tot);
            int total = Integer.parseInt(tot.toString());

            root.setPercentage(total);

        } catch (NumberFormatException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        Map<String, CoverResults> results = prepareData(res);

        for (String name : results.keySet()) {
            CoverResults unitRes = results.get(name);
            IStatsTreeObject obj = new FunctionStats(name,
                    unitRes.linesTotal, unitRes.linesCovered,
                    unitRes.percentage);
            root.addChild(obj);
        }

    }

    private Map<String, CoverResults> prepareData(OtpErlangTuple res) {

        Map<String, CoverResults> results = new HashMap<String, CoverResults>();

        OtpErlangList list = (OtpErlangList) ((OtpErlangTuple) res.elementAt(1))
                .elementAt(1);
        System.out.println("2: " + list);

        Iterator<OtpErlangObject> it = list.iterator();
        while (it.hasNext()) {
            OtpErlangTuple fileTuple = (OtpErlangTuple) it.next();
            System.out.println("3: " + fileTuple);
            String name = fileTuple.elementAt(0).toString();
            System.out.println("4: " + name);
            CoverResults unitRes = new CoverResults();

            unitRes.percentage = Integer.parseInt(fileTuple.elementAt(1)
                    .toString());
            System.out.println("5: " + unitRes.percentage);

            results.put(name, unitRes);

        }

        OtpErlangList list2 = (OtpErlangList) ((OtpErlangTuple) res
                .elementAt(2)).elementAt(1);
        System.out.println("6: " + list2);

        it = list2.iterator();
        while (it.hasNext()) {
            OtpErlangTuple fileTuple = (OtpErlangTuple) it.next();
            System.out.println("7: " + fileTuple);
            String name = fileTuple.elementAt(0).toString();
            System.out.println("8: " + name);
            CoverResults unitRes = results.get(name);

            OtpErlangTuple lines = (OtpErlangTuple) fileTuple.elementAt(1);
            System.out.println("9: " + lines);

            unitRes.linesTotal = Integer
                    .parseInt(lines.elementAt(0).toString());
            System.out.println("10: " + unitRes.linesTotal);
            unitRes.linesCovered = Integer.parseInt(lines.elementAt(1)
                    .toString());
            System.out.println("11: " + unitRes.linesCovered);

        }

        return results;

    }*/

}
