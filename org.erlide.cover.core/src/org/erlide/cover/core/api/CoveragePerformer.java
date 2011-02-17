package org.erlide.cover.core.api;

import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.erlide.cover.core.CoverBackend;
import org.erlide.cover.core.CoverEvent;
import org.erlide.cover.core.CoverStatus;
import org.erlide.cover.core.ICoverObserver;
import org.erlide.cover.views.model.StatsTreeModel;

public class CoveragePerformer implements CoverAPI {
    
    private static CoveragePerformer performer;
    
    private Collection<String> coverNodes;
    private IConfiguration config;
    
    private Logger log;     //logger
    
    private CoveragePerformer() {
        log = Logger.getLogger(this.getClass());
        coverNodes = new LinkedList<String>();
    }
    
    public static synchronized CoveragePerformer getPerformer() {
        if(performer == null)
            performer = new CoveragePerformer();
        return performer;
    }

    public synchronized void startCover(Collection<String> nodes) {
        
        final StatsTreeModel model = StatsTreeModel.getInstance();
        model.clear();
        CoverBackend.getInstance().getAnnotationMaker().clearAllAnnotations();
        
        for (final ICoverObserver obs : CoverBackend.getInstance().getListeners()) {
            obs.eventOccured(new CoverEvent(CoverStatus.UPDATE));
        }
        
        boolean different = false;
        for(String node : nodes)
            if(!coverNodes.contains(node)) {
                different = true;
                break;
            }
        
        if(coverNodes.isEmpty() || different) {

            coverNodes = nodes;
            log.info(CoverBackend.getInstance().getBackend().getFullNodeName());
            coverNodes.add(CoverBackend.getInstance().getBackend().getFullNodeName());
            
            //TODO: restart cover on nodes
            //send mesg about restarting
        }
        
        
    }
    
    public synchronized void setCoverageConfiguration(IConfiguration conf) {
        config = conf;
        
    }
    
    public synchronized void analyse() {      
        //TODO: mesg about analysis
    }
    
}
