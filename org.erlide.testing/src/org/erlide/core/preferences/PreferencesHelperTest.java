package org.erlide.core.preferences;

import junit.framework.Assert;

import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.osgi.service.prefs.BackingStoreException;

public class PreferencesHelperTest {

	private static final String QUALIFIER = "org.erlide.testing";

	@Before
	public void setUp() {
		try {
			new InstanceScope().getNode(QUALIFIER).removeNode();
			new InstanceScope().getNode(QUALIFIER).flush();
			new ConfigurationScope().getNode(QUALIFIER).removeNode();
			new ConfigurationScope().getNode(QUALIFIER).flush();
			new DefaultScope().getNode(QUALIFIER).removeNode();
			new DefaultScope().getNode(QUALIFIER).flush();
		} catch (BackingStoreException e) {
		}
	}

	@After
	public void tearDown() {
		try {
			new InstanceScope().getNode(QUALIFIER).removeNode();
			new InstanceScope().getNode(QUALIFIER).flush();
			new ConfigurationScope().getNode(QUALIFIER).removeNode();
			new ConfigurationScope().getNode(QUALIFIER).flush();
			new DefaultScope().getNode(QUALIFIER).removeNode();
			new DefaultScope().getNode(QUALIFIER).flush();
		} catch (BackingStoreException e) {
		}
	}

	@Test
	public void nextContexts_1() {
		IScopeContext[] list = new IScopeContext[] { new InstanceScope(),
				new ConfigurationScope(), new DefaultScope() };
		IScopeContext item = new ConfigurationScope();
		IScopeContext[] val = PreferencesHelper.getNextContexts(list, item);
		Assert.assertNotNull(val);
		Assert.assertEquals(1, val.length);
		Assert.assertEquals(new DefaultScope(), val[0]);
	}

	@Test
	public void nextContexts_2() {
		IScopeContext[] list = new IScopeContext[] { new InstanceScope(),
				new ConfigurationScope(), new DefaultScope() };
		IScopeContext item = new DefaultScope();
		IScopeContext[] val = PreferencesHelper.getNextContexts(list, item);
		Assert.assertNotNull(val);
		Assert.assertEquals(0, val.length);
	}

	@Test
	public void nextContexts_3() {
		IScopeContext[] list = new IScopeContext[] { new InstanceScope(),
				new ConfigurationScope(), new DefaultScope() };
		IScopeContext item = new InstanceScope();
		IScopeContext[] val = PreferencesHelper.getNextContexts(list, item);
		Assert.assertNotNull(val);
		Assert.assertEquals(2, val.length);
		Assert.assertEquals(new ConfigurationScope(), val[0]);
		Assert.assertEquals(new DefaultScope(), val[1]);
	}

	@Test
	public void simpleSet() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		String key = "val1";
		helper.putString(key, "smurf");
		String res = helper.getString(key, "default");
		Assert.assertEquals("smurf", res);
	}

	@Test
	public void simpleSet_default_1() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		String key = "val2";
		new DefaultScope().getNode(QUALIFIER).put(key, "balm");
		helper.putString(key, "balm");
		String res = new InstanceScope().getNode(QUALIFIER).get(key, null);
		Assert.assertNull(res);
		res = new ConfigurationScope().getNode(QUALIFIER).get(key, null);
		Assert.assertNull(res);
		res = new DefaultScope().getNode(QUALIFIER).get(key, null);
		Assert.assertNotNull(res);
	}

	@Test
	public void simpleSet_default_2() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		String key = "val3";
		new ConfigurationScope().getNode(QUALIFIER).put(key, "balm");
		helper.putString(key, "balm");
		String res = new InstanceScope().getNode(QUALIFIER).get(key, null);
		Assert.assertNull(res);
		res = new ConfigurationScope().getNode(QUALIFIER).get(key, null);
		Assert.assertNotNull(res);
		res = new DefaultScope().getNode(QUALIFIER).get(key, null);
		Assert.assertNull(res);
	}

	@Test
	public void simpleSet_byteArray() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		String key = "val4";
		byte[] val = new byte[] { 1, 3, 5, 7 };
		helper.putByteArray(key, val);
		String zz = helper.getString(key, null);
		byte[] res = helper.getByteArray(key, null);
		Assert.assertEquals("smurf", res);
	}
}
