package org.erlide.eunit.core;

import org.erlide.jinterface.backend.events.EventHandler;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class EUnitEventHandler extends EventHandler {
	private static final String EVENT_NAME = "cover_event";
	
	private static final String ATOM_COVER_OK = "cover_ok";
	private static final String ATOM_COVER_ERROR = "cover_error";
	
	
	
	@Override
	protected void doHandleMsg(OtpErlangObject msg) throws Exception {
		OtpErlangObject event = getStandardEvent(msg, EVENT_NAME);
        OtpErlangObject errorReason = null;
        
        System.out.println("Some mesg " + msg);
        
        if (event == null) 
        	return;
        
        System.out.println(event.toString());
        
        if( isCoveringFinished(event) ){
        	
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
            	//sending to statistics
            	System.out.println(mesgTuple.toString());
            	
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
