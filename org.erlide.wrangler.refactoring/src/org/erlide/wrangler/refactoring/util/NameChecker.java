package org.erlide.wrangler.refactoring.util;

import com.ericsson.otp.erlang.OtpErlangAtom;

public class NameChecker {
	
	private NameChecker() {}
	
	public static boolean checkIsAtom(String s) {
		if (s.length() == 0) {
			return false;	
		}
		if(s.charAt(0) == '\'' && s.charAt(s.length()-1) == '\'') {
			return true;
		}
		else {
			if (s.substring(0,1).replaceAll("[a-z]", "").isEmpty() && s.replaceAll("[A-Za-z_@0-9]", "").isEmpty()) {
				return true;
			} else {
				return false;
			}
		}
	}
	
	public static boolean checkIsVariable(String s) {
		if(s.length() == 0) {
			return false;
		}
		
		if(s.startsWith("_") || s.substring(0,1).toUpperCase().equals(s.substring(0, 1)) ) {
			if(s.replaceAll("[A-Za-z_@0-9]", "").isEmpty()) {
				return true;
			}
			else {
				return false;
			}
		} else {
			return false;
		}
	}


}
