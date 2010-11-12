package org.erlide.eunit.core;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.erlide.jinterface.backend.events.EventHandler;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class EUnitEventHandler extends EventHandler {
	private static final String EVENT_NAME = "cover_event";
	
	private static final String ATOM_COVER_OK = "cover_ok";
	private static final String ATOM_COVER_ERROR = "cover_error";
	
	private EUnitBackend backend;
	
	private int total;
	private Map<String, CoverResults> files;
	private boolean finished = false;
	
	public EUnitEventHandler(EUnitBackend backend){
		this.backend = backend;
	}
	
	public int getTotal(){
		return total;
	}
	
	public Map<String, CoverResults> getResults(){
		return files;
	}
	
	public boolean isFinished() {
		return finished;
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
        	finished = true;
        	for( IEUnitObserver obs: backend.getListeners())
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
            	
            	try {
            		System.out.println("1: " + res.elementAt(0));
            		System.out.println("1: " + ((OtpErlangTuple)res.elementAt(0)).elementAt(1));
            		OtpErlangObject tot = ((OtpErlangTuple)res.elementAt(0)).elementAt(1);
            		System.out.println("1: " + tot);
					total = Integer.parseInt(tot.toString());
					
				} catch (NumberFormatException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} 
				
				files = new HashMap<String, CoverResults>();
				OtpErlangList list = (OtpErlangList)((OtpErlangTuple)res.elementAt(1)).elementAt(1);
				System.out.println("2: " + list);
				
				Iterator<OtpErlangObject> it = list.iterator();
				while(it.hasNext()){
					OtpErlangTuple fileTuple = (OtpErlangTuple)it.next();
					System.out.println("3: " + fileTuple);
					String name = fileTuple.elementAt(0).toString();
					System.out.println("4: " + name);
					CoverResults results = new CoverResults();
					
						results.percent  = Integer.parseInt(fileTuple.elementAt(1).toString());
						System.out.println("5: " + results.percent);
					
				
					files.put(name, results);
				}
				
				OtpErlangList list2 = (OtpErlangList)((OtpErlangTuple)res.elementAt(2)).elementAt(1);
				System.out.println("6: " + list2);
				
				it = list2.iterator();
				while(it.hasNext()){
					OtpErlangTuple fileTuple = (OtpErlangTuple)it.next();
					System.out.println("7: " + fileTuple);
					String name = fileTuple.elementAt(0).toString();
					System.out.println("8: " + name);
					CoverResults results = files.get(name);
					
					OtpErlangTuple lines = (OtpErlangTuple)fileTuple.elementAt(1);
					System.out.println("9: " + lines);
	
						results.linesTotal = Integer.parseInt(lines.elementAt(0).toString());	
						System.out.println("10: " + results.linesTotal);
						results.linesCovered = Integer.parseInt(lines.elementAt(1).toString());
						System.out.println("11: " + results.linesCovered);
					
					
				}
				
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
	
	

}
