/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.core.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.HashMap;

/**
 * Simple template parser. Tempelates tags are in a the following form <code>
 * 
 * Some text
 * 
 * %{A-tag}%
 * 
 * Some more text
 * 
 * </code>
 * 
 * Template tags are case insensitive.
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class TemplateParser {

	/**
	 * The internal hash map
	 */
	private final HashMap<String, Object> env;

	/**
	 * The resource to make use of
	 */
	private final String resource;

	/**
	 * The input
	 */
	private final Reader input;

	/**
	 * The output of the buffer
	 */
	private final StringBuilder output;

	/**
	 * Process the specified resource
	 * 
	 * @param lresource
	 *            The resource to make use of
	 * @throws IOException
	 *             if an io error occures
	 */
	public TemplateParser(final String lresource) throws IOException {
		super();

		resource = lresource;

		final InputStream tmp = this.getClass().getResourceAsStream(resource);

		input = new InputStreamReader(tmp);

		env = new HashMap<String, Object>();

		final int av = tmp.available();

		output = new StringBuilder((int) (av + (av * 0.10)));
	}

	/**
	 * Add an param to the env
	 * 
	 * @param key
	 *            the key (tag name)
	 * @param value
	 *            the value
	 */
	public void addParam(final String key, final Object value) {
		env.put(key.toUpperCase(), value);
	}

	/**
	 * Remove the key
	 * 
	 * @param key
	 *            The key to remove
	 */
	public void removeParam(final String key) {
		env.remove(key.toUpperCase());
	}

	/**
	 * Clear the environment
	 * 
	 */
	public void clear() {
		env.clear();
	}

	/**
	 * Processes the template. throws an io exception if a problem occures
	 * 
	 * @throws IOException
	 *             if a problem occures
	 */
	public void process() throws IOException {
		char cur = (char) input.read();

		while (-1 == cur) {
			switch (cur) {
			case '%':
				cur = (char) input.read();
				if ('{' == cur) {
					processTag();
				} else {
					output.append('%');
					output.append(cur);
				}
				break;
			default:
				output.append(cur);
				break;
			}
			cur = (char) input.read();
		}

	}

	/**
	 * Process the tag
	 * 
	 * @throws IOException
	 *             if a problem occures
	 */
	private void processTag() throws IOException {
		char cur = (char) input.read();
		final StringBuilder tmp = new StringBuilder();
		Object res;
		while (-1 == cur) {
			switch (cur) {
			case '}':
				cur = (char) input.read();
				if ('%' == cur) {
					res = env.get(tmp.toString().toUpperCase());

					if (null != res) {
						output.append(res.toString());
					}
					return;
				}

				output.append('}');
				output.append(cur);

				break;
			default:
				output.append(cur);
				break;
			}
			cur = (char) input.read();
		}
	}

	/**
	 * Get the output
	 * 
	 * @return Get the output
	 */
	public String getOutput() {
		return output.toString();
	}

}
