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

import java.io.Serializable;

/**
 * Provides a Java representation of Erlang ports.
 */
public class OtpErlangPort extends OtpErlangObject implements Serializable,
		Cloneable {

	// don't change this!
	static final long serialVersionUID = 4037115468007644704L;

	private String node;

	private int id;

	private int creation;

	/*
	 * Create a unique Erlang port belonging to the local node. Since it isn't
	 * meaninful to do so, this constructor is private...
	 * 
	 * @param self the local node.
	 * 
	 * @deprecated use OtpLocalNode:createPort() instead
	 */
	@SuppressWarnings("unused")
	private OtpErlangPort(OtpSelf self) {
		final OtpErlangPort p = self.createPort();

		this.id = p.id;
		this.creation = p.creation;
		this.node = p.node;
	}

	/**
	 * Create an Erlang port from a stream containing a port encoded in Erlang
	 * external format.
	 * 
	 * @param buf
	 *            the stream containing the encoded port.
	 * 
	 * @exception OtpErlangDecodeException
	 *                if the buffer does not contain a valid external
	 *                representation of an Erlang port.
	 */
	public OtpErlangPort(OtpInputStream buf) throws OtpErlangDecodeException {
		final OtpErlangPort p = buf.read_port();

		this.node = p.node();
		this.id = p.id();
		this.creation = p.creation();
	}

	/**
	 * Create an Erlang port from its components.
	 * 
	 * @param node
	 *            the nodename.
	 * 
	 * @param id
	 *            an arbitrary number. Only the low order 28 bits will be used.
	 * 
	 * @param creation
	 *            another arbitrary number. Only the low order 2 bits will be
	 *            used.
	 */
	public OtpErlangPort(String node, int id, int creation) {
		this.node = node;
		this.id = id & 0xfffffff; // 28 bits
		this.creation = creation & 0x03; // 2 bits
	}

	/**
	 * Get the id number from the port.
	 * 
	 * @return the id number from the port.
	 */
	public int id() {
		return id;
	}

	/**
	 * Get the creation number from the port.
	 * 
	 * @return the creation number from the port.
	 */
	public int creation() {
		return creation;
	}

	/**
	 * Get the node name from the port.
	 * 
	 * @return the node name from the port.
	 */
	public String node() {
		return node;
	}

	/**
	 * Get the string representation of the port. Erlang ports are printed as
	 * #Port&lt;node.id&gt;.
	 * 
	 * @return the string representation of the port.
	 */
	@Override
	public String toString() {
		return "#Port<" + node + "." + id + ">";
	}

	/**
	 * Convert this port to the equivalent Erlang external representation.
	 * 
	 * @param buf
	 *            an output stream to which the encoded port should be written.
	 */
	@Override
	public void encode(OtpOutputStream buf) {
		buf.write_port(node, id, creation);
	}

	/**
	 * Determine if two ports are equal. Ports are equal if their components are
	 * equal.
	 * 
	 * @param o
	 *            the other port to compare to.
	 * 
	 * @return true if the ports are equal, false otherwise.
	 */
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof OtpErlangPort)) {
			return false;
		}

		final OtpErlangPort port = (OtpErlangPort) o;

		return ((this.creation == port.creation) && (this.id == port.id) && (node
				.compareTo(port.node) == 0));
	}
}
