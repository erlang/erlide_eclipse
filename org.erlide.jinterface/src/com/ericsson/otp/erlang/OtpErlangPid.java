/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 *
 *     $Id$
 */
package com.ericsson.otp.erlang;


/**
 * Provides a Java representation of Erlang PIDs. PIDs represent Erlang
 * processes and consist of a nodename and a number of integers.
 */
@SuppressWarnings("unchecked")
public class OtpErlangPid extends OtpErlangObject implements Comparable {

	// don't change this!
	static final long serialVersionUID = 1664394142301803659L;

	private String node;

	private int id;

	private int serial;

	private int creation;

	/**
	 * Create a unique Erlang PID belonging to the local node.
	 * 
	 * @param self
	 * 		the local node.
	 * 
	 * @deprecated use OtpLocalNode:createPid() instead
	 */
	@Deprecated
	public OtpErlangPid(OtpLocalNode self) {
		final OtpErlangPid p = self.createPid();

		id = p.id;
		serial = p.serial;
		creation = p.creation;
		node = p.node;
	}

	/**
	 * Create an Erlang PID from a stream containing a PID encoded in Erlang
	 * external format.
	 * 
	 * @param buf
	 * 		the stream containing the encoded PID.
	 * 
	 * @exception OtpErlangDecodeException
	 * 		if the buffer does not contain a valid external representation of an
	 * 		Erlang PID.
	 */
	public OtpErlangPid(OtpInputStream buf) throws OtpErlangDecodeException {
		final OtpErlangPid p = buf.read_pid();

		node = p.node();
		id = p.id();
		serial = p.serial();
		creation = p.creation();
	}

	/**
	 * Create an Erlang pid from its components.
	 * 
	 * @param node
	 * 		the nodename.
	 * 
	 * @param id
	 * 		an arbitrary number. Only the low order 15 bits will be used.
	 * 
	 * @param serial
	 * 		another arbitrary number. Only the low order 13 bits will be used.
	 * 
	 * @param creation
	 * 		yet another arbitrary number. Only the low order 2 bits will be
	 * 		used.
	 */
	public OtpErlangPid(String node, int id, int serial, int creation) {
		this.node = node;
		this.id = id & 0x7fff; // 15 bits
		this.serial = serial & 0x1fff; // 13 bits
		this.creation = creation & 0x03; // 2 bits
	}

	/**
	 * Get the serial number from the PID.
	 * 
	 * @return the serial number from the PID.
	 */
	public int serial() {
		return serial;
	}

	/**
	 * Get the id number from the PID.
	 * 
	 * @return the id number from the PID.
	 */
	public int id() {
		return id;
	}

	/**
	 * Get the creation number from the PID.
	 * 
	 * @return the creation number from the PID.
	 */
	public int creation() {
		return creation;
	}

	/**
	 * Get the node name from the PID.
	 * 
	 * @return the node name from the PID.
	 */
	public String node() {
		return node;
	}

	/**
	 * Get the string representation of the PID. Erlang PIDs are printed as
	 * #Pid&lt;node.id.serial&gt;
	 * 
	 * @return the string representation of the PID.
	 */
	@Override
	public String toString() {
		return "#Pid<" + node + "." + id + "." + serial + ">";
	}

	/**
	 * Convert this PID to the equivalent Erlang external representation.
	 * 
	 * @param buf
	 * 		an output stream to which the encoded PID should be written.
	 */
	@Override
	public void encode(OtpOutputStream buf) {
		buf.write_pid(node, id, serial, creation);
	}

	/**
	 * Return the hashCode for this Pid.
	 * 
	 * @return the hashCode for this Pid.
	 */
	@Override
	public int hashCode() {
		return (creation * 17 + serial * 23 + id * 31 + node.hashCode()) & 0xFFFFFF;
	}

	/**
	 * Determine if two PIDs are equal. PIDs are equal if their components are
	 * equal.
	 * 
	 * @param port
	 * 		the other PID to compare to.
	 * 
	 * @return true if the PIDs are equal, false otherwise.
	 */
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof OtpErlangPid)) {
			return false;
		}

		final OtpErlangPid pid = (OtpErlangPid) o;

		return ((creation == pid.creation) && (serial == pid.serial)
				&& (id == pid.id) && (node.compareTo(pid.node) == 0));
	}

	public int compareTo(Object o) {
		if (!(o instanceof OtpErlangPid)) {
			return -1;
		}

		final OtpErlangPid pid = (OtpErlangPid) o;

		if (equals(o)) {
			return 0;
		}
		return ((creation > pid.creation) || (serial > pid.serial)
				|| (id > pid.id) || (node.compareTo(pid.node) > 0)) ? 1 : -1;
	}
}
