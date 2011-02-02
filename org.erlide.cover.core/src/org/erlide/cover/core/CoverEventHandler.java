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
    private static final String COVER_FIN = "cover_fin";
    private static final String COVER_ERROR = "cover_error";
    private static final String COVER_RES = "module_res";
    private static final String INDEX = "index";

    private List<ICoverObserver> listeners = new LinkedList<ICoverObserver>();
    private int moduleNum;
    private int counter;
    private ICoverAnnotationMarker annotationMarker;


    public void addListener(ICoverObserver listener) {
        System.out.println("adding listener");
        listeners.add(listener);
    }

    public List<ICoverObserver> getListeners() {
        return listeners;
    }
    
    public void addAnnotationMaker(ICoverAnnotationMarker am) {
        annotationMarker = am;
    }
    
    public ICoverAnnotationMarker getAnnotationMaker() {
        return annotationMarker;
    }
    

    @Override
    protected void doHandleMsg(OtpErlangObject msg) throws Exception {
        OtpErlangObject event = getStandardEvent(msg, EVENT_NAME);
        OtpErlangTuple tuple = null;

        System.out.println("Received mesg " + msg);

        if (event == null)
            return;

        System.out.println(event.toString());

        if (gotResults(event)) {
            for (ICoverObserver obs : listeners)
                obs.eventOccured(new CoverEvent(CoverStatus.UPDATE));
            System.out.println("Got results!");
        } else if ((tuple = getErrorReason(event)) != null) {
            String place = tuple.elementAt(1).toString();
            String type = tuple.elementAt(2).toString();
            String info = tuple.elementAt(3).toString();
            
            ErlLogger.debug("Mesg error");
            
            for (ICoverObserver obs : listeners)
                obs.eventOccured(new CoverEvent(CoverStatus.ERROR,
                        String.format("Error at %s while %s: %s\n",
                                place, type, info)));
            System.out.println("Got results!");
        } else if (event.toString().equals(COVER_FIN)) {
            System.out.println("add Annotations");
            getAnnotationMaker().addAnnotations();
        }

    }

    /**
     * @deprecated
     * @param msg
     * @return
     */
    private boolean gotIndex(OtpErlangObject msg) {
        if( msg instanceof OtpErlangTuple) {
            OtpErlangTuple resTuple = (OtpErlangTuple) msg;
            if(resTuple.elementAt(0) instanceof OtpErlangAtom &&
                    resTuple.elementAt(0).toString().equals(INDEX)) {
                
                String htmlPath = resTuple.elementAt(1).toString();
                System.out.println(htmlPath);
                StatsTreeModel.getInstance().setIndex(htmlPath.substring(1,
                        htmlPath.toString().length() -1));
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
                htmlPath = htmlPath.substring(1, htmlPath.length()-1);
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
              
                ICoverageStats moduleOld = root.findChild(moduleStats.getLabel());
                if(moduleOld != null) {
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
            
            stats.addChild(func.getLabel(), func);
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

    private OtpErlangTuple getErrorReason(OtpErlangObject message) {
        if (message instanceof OtpErlangTuple) {
            OtpErlangTuple tuple = (OtpErlangTuple) message;
            if (tuple.elementAt(0) instanceof OtpErlangAtom
                    && ((OtpErlangAtom) tuple.elementAt(0)).atomValue().equals(
                            COVER_ERROR)) {
                
                

                return tuple;
            }
        }
        return null;
    }

}
