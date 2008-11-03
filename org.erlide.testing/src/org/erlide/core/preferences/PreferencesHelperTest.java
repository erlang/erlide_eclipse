package org.erlide.core.preferences;

import java.util.Arrays;

import junit.framework.Assert;

import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.junit.Before;
import org.junit.Test;
import org.osgi.service.prefs.BackingStoreException;

public class PreferencesHelperTest {

	private static final String QUALIFIER = "org.erlide.testing";
	private static final String KEY = "val";

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
	public void string_set() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		helper.putString(KEY, "smurf");
		String res = helper.getString(KEY, "default");
		Assert.assertEquals("smurf", res);
	}

	@Test
	public void string_default_0() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		helper.putString(KEY, "balm");
		String res = new InstanceScope().getNode(QUALIFIER).get(KEY, null);
		Assert.assertNotNull(res);
		res = new ConfigurationScope().getNode(QUALIFIER).get(KEY, null);
		Assert.assertNull(res);
		res = new DefaultScope().getNode(QUALIFIER).get(KEY, null);
		Assert.assertNull(res);
	}

	@Test
	public void string_default_1() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		new DefaultScope().getNode(QUALIFIER).put(KEY, "balm");
		helper.putString(KEY, "balm");
		String res = new InstanceScope().getNode(QUALIFIER).get(KEY, null);
		Assert.assertNull(res);
		res = new ConfigurationScope().getNode(QUALIFIER).get(KEY, null);
		Assert.assertNull(res);
		res = new DefaultScope().getNode(QUALIFIER).get(KEY, null);
		Assert.assertNotNull(res);
	}

	@Test
	public void string_default_2() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		new ConfigurationScope().getNode(QUALIFIER).put(KEY, "balm");
		helper.putString(KEY, "balm");
		String res = new InstanceScope().getNode(QUALIFIER).get(KEY, null);
		Assert.assertNull(res);
		res = new ConfigurationScope().getNode(QUALIFIER).get(KEY, null);
		Assert.assertNotNull(res);
		res = new DefaultScope().getNode(QUALIFIER).get(KEY, null);
		Assert.assertNull(res);
	}

	@Test
	public void byteArray_set() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		byte[] val = new byte[] { 1, 3, 5, 7 };
		helper.putByteArray(KEY, val);
		byte[] res = helper.getByteArray(KEY, null);
		Assert.assertEquals(Arrays.toString(val), Arrays.toString(res));
	}

	@Test
	public void double_set() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		double val = 3.1415926d;
		helper.putDouble(KEY, val);
		double res = helper.getDouble(KEY, Double.NaN);
		Assert.assertEquals(val, res, 1e-5);
	}

	@Test
	public void float_set() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		float val = 3.1415926f;
		helper.putFloat(KEY, val);
		float res = helper.getFloat(KEY, Float.NaN);
		Assert.assertEquals(val, res, 1e-5);
	}

	@Test
	public void long_set() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		long val = 314159260;
		helper.putLong(KEY, val);
		long res = helper.getLong(KEY, Long.MIN_VALUE);
		Assert.assertEquals(val, res);
	}

	@Test
	public void int_set() {
		PreferencesHelper helper = new PreferencesHelper(QUALIFIER,
				new InstanceScope());
		int val = 314159;
		helper.putInt(KEY, val);
		int res = helper.getInt(KEY, Integer.MIN_VALUE);
		Assert.assertEquals(val, res);
	}
}
