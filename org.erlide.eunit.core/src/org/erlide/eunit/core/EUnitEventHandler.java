package org.erlide.eunit.core;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.erlide.eunit.views.model.IStatsTreeObject;
import org.erlide.eunit.views.model.StatsTreeModel;
import org.erlide.eunit.views.model.StatsTreeObject;
import org.erlide.jinterface.backend.events.EventHandler;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class EUnitEventHandler extends EventHandler {
	private static final String EVENT_NAME = "cover_event";
	
	private static final String ATOM_COVER_OK = "cover_ok";
	private static final String ATOM_COVER_ERROR = "cover_error";
	private List<IEUnitObserver> listeners = new LinkedList<IEUnitObserver>();
	
	//private EUnitBackend backend;
	
//	private int total;
//	private Map<String, CoverResultsPerObject> files;
//	private boolean finished = false;
	
//	public EUnitEventHandler(EUnitBackend backend){
//		this.backend = backend;
//	}
	
/*	public int getTotal(){
		return total;
	}
	
	public Map<String, CoverResultsPerObject> getResults(){
		return files;
	}*/
	
/*	public boolean isFinished() {
		return finished;
	}*/
	
	public void addListener(IEUnitObserver listener) {
		System.out.println("adding listener");
		listeners.add(listener);
	}
	
	public List<IEUnitObserver> getListeners() {
		return listeners;
	}
	
	@Override
	protected void doHandleMsg(OtpErlangObject msg) throws Exception {
		OtpErlangObject event = getStandardEvent(msg, EVENT_NAME);
        OtpErlangObject errorReason = null;
        
        System.out.println("Some mesg " + msg);
        
        if (event == null) 
        	return;
        
        System.out.println(event.toString());
        
        if( isCoveringFinished(event) ){
        //	finished = true;
        	for( IEUnitObserver obs: listeners)
        		obs.finishCovering();
        	System.out.println("Finish covering!!");
        } else if ((errorReason = getErrorReson(event)) != null) {
        	
        } else if ( isResponding(event) ){
        	System.out.println("Responding...");
        }
	}
	
	
	private boolean isResponding(OtpErlangObject msg) {
		if(msg instanceof OtpErlangAtom && 
				((OtpErlangAtom)msg).atomValue().equals("ok")){
			return true;
		}
		return false;
	}
	
	private boolean isCoveringFinished(OtpErlangObject msg) {
		if (msg instanceof OtpErlangTuple) { 	
            OtpErlangTuple mesgTuple = (OtpErlangTuple) msg;
            if(mesgTuple.elementAt(0) instanceof OtpErlangAtom &&
            		((OtpErlangAtom)mesgTuple.elementAt(0)).
            		atomValue().equals(ATOM_COVER_OK)) {
            	
            	ErlLogger.debug("Mesg ok");
            	
            	System.out.println(mesgTuple.toString());
            	
            	OtpErlangTuple res = (OtpErlangTuple)mesgTuple.elementAt(1);
            	System.out.println("0:: " + res);
            	
            	changeStatModel(res);
				
                return true;
            }
        }
        return false;
	}
	
	private OtpErlangObject getErrorReson(OtpErlangObject message) {
        if (message instanceof OtpErlangTuple) { 
            OtpErlangTuple tuple = (OtpErlangTuple) message;
            if (tuple.elementAt(0) instanceof OtpErlangAtom && 
            		((OtpErlangAtom) tuple.elementAt(0)).
            		atomValue().equals(ATOM_COVER_ERROR)) {
            	
            	ErlLogger.debug("Mesg error");
            	
                return tuple.elementAt(1);
            }
        }
        return null;
    }
	
	private void changeStatModel(OtpErlangTuple res) {
		
		
		
		StatsTreeModel model = StatsTreeModel.getInstance();
		//TODO: shoudl be changed at some point probably
		IStatsTreeObject root = model.getRoot();
		root.removeAllChildren();
		
		try {
    		System.out.println("1: " + res.elementAt(0));
    		System.out.println("1: " + ((OtpErlangTuple)res.elementAt(0)).elementAt(1));
    		OtpErlangObject tot = ((OtpErlangTuple)res.elementAt(0)).elementAt(1);
    		System.out.println("1: " + tot);
			int total = Integer.parseInt(tot.toString());
			
			root.setPercentage(total);
			
		} catch (NumberFormatException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
		
		Map<String, CoverResults> results = prepareData(res);
		
		for(String name : results.keySet()) {
			CoverResults unitRes = results.get(name);
			IStatsTreeObject obj = new StatsTreeObject(name, unitRes.linesTotal, unitRes.linesCovered, unitRes.percentage);
			root.addChild(obj);
		}
		
	}
	
	private Map<String, CoverResults> prepareData(OtpErlangTuple res) {
		
		Map<String, CoverResults> results = new HashMap<String, CoverResults>();
		
		OtpErlangList list = (OtpErlangList)((OtpErlangTuple)res.elementAt(1)).elementAt(1);
		System.out.println("2: " + list);
		
		Iterator<OtpErlangObject> it = list.iterator();
		while(it.hasNext()){
			OtpErlangTuple fileTuple = (OtpErlangTuple)it.next();
			System.out.println("3: " + fileTuple);
			String name = fileTuple.elementAt(0).toString();
			System.out.println("4: " + name);
			CoverResults unitRes = new CoverResults();
			
			unitRes.percentage  = Integer.parseInt(fileTuple.elementAt(1).toString());
			System.out.println("5: " + unitRes.percentage);
			
			results.put(name, unitRes);
		
		}
		
		OtpErlangList list2 = (OtpErlangList)((OtpErlangTuple)res.elementAt(2)).elementAt(1);
		System.out.println("6: " + list2);
		
		it = list2.iterator();
		while(it.hasNext()){
			OtpErlangTuple fileTuple = (OtpErlangTuple)it.next();
			System.out.println("7: " + fileTuple);
			String name = fileTuple.elementAt(0).toString();
			System.out.println("8: " + name);
			CoverResults unitRes = results.get(name);
			
			OtpErlangTuple lines = (OtpErlangTuple)fileTuple.elementAt(1);
			System.out.println("9: " + lines);

				unitRes.linesTotal = Integer.parseInt(lines.elementAt(0).toString());	
				System.out.println("10: " + unitRes.linesTotal);
				unitRes.linesCovered = Integer.parseInt(lines.elementAt(1).toString());
				System.out.println("11: " + unitRes.linesCovered);
			
			
		}
		
		return results;
		
	}
	

}
