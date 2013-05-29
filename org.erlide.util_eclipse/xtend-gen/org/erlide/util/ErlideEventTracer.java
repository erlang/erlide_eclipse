package org.erlide.util;

import java.util.Dictionary;
import java.util.HashMap;
import java.util.Hashtable;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.util.ErlideCrashEvent;
import org.erlide.util.ErlideEvent;
import org.erlide.util.ErlideOperationEvent;
import org.erlide.util.ErlideResetEvent;
import org.erlide.util.ErlideSessionEvent;
import org.erlide.util.ErlideStatusEvent;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;
import org.osgi.util.tracker.ServiceTracker;

@SuppressWarnings("all")
public class ErlideEventTracer implements BundleActivator {
  public final static String ERLIDE_EVENT_TOPIC = "org/erlide/erlide_event";
  
  private ServiceTracker<Object,Object> tracker;
  
  private final static ErlideEventTracer instance = new Function0<ErlideEventTracer>() {
    public ErlideEventTracer apply() {
      ErlideEventTracer _erlideEventTracer = new ErlideEventTracer();
      return _erlideEventTracer;
    }
  }.apply();
  
  private static boolean hasHandlers = false;
  
  public void start(final BundleContext context) throws Exception {
    String _name = EventAdmin.class.getName();
    ServiceTracker<Object,Object> _serviceTracker = new ServiceTracker<Object,Object>(context, _name, null);
    this.tracker = _serviceTracker;
    this.tracker.open();
  }
  
  public void stop(final BundleContext context) throws Exception {
    boolean _tripleNotEquals = (this.tracker != null);
    if (_tripleNotEquals) {
      this.tracker.close();
    }
  }
  
  public static void doStart(final BundleContext context) {
    try {
      ErlideEventTracer.instance.start(context);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static void doStop(final BundleContext context) {
    try {
      ErlideEventTracer.instance.stop(context);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static void traceSession() {
    ErlideSessionEvent _erlideSessionEvent = new ErlideSessionEvent();
    ErlideEventTracer.instance.trace(_erlideSessionEvent);
  }
  
  public static void traceReset() {
    ErlideResetEvent _erlideResetEvent = new ErlideResetEvent();
    ErlideEventTracer.instance.trace(_erlideResetEvent);
  }
  
  public static void traceCrash(final String backend) {
    ErlideCrashEvent _erlideCrashEvent = new ErlideCrashEvent(backend);
    ErlideEventTracer.instance.trace(_erlideCrashEvent);
  }
  
  public static void traceStatus(final Object status) {
    ErlideStatusEvent _erlideStatusEvent = new ErlideStatusEvent(status);
    ErlideEventTracer.instance.trace(_erlideStatusEvent);
  }
  
  public static void traceOperation(final String operation, final long duration) {
    ErlideOperationEvent _erlideOperationEvent = new ErlideOperationEvent(operation, duration);
    ErlideEventTracer.instance.trace(_erlideOperationEvent);
  }
  
  private void trace(final ErlideEvent event) {
    boolean _or = false;
    boolean _tripleEquals = (this.tracker == null);
    if (_tripleEquals) {
      _or = true;
    } else {
      boolean _not = (!ErlideEventTracer.hasHandlers);
      _or = (_tripleEquals || _not);
    }
    if (_or) {
      return;
    }
    Object _service = this.tracker.getService();
    final EventAdmin ea = ((EventAdmin) _service);
    boolean _tripleNotEquals = (ea != null);
    if (_tripleNotEquals) {
      Pair<String,ErlideEvent> _mappedTo = Pair.<String, ErlideEvent>of("event", event);
      HashMap<String,ErlideEvent> _newHashMap = CollectionLiterals.<String, ErlideEvent>newHashMap(_mappedTo);
      Event _event = new Event(ErlideEventTracer.ERLIDE_EVENT_TOPIC, _newHashMap);
      final Event evt = _event;
      ea.postEvent(evt);
    }
  }
  
  public static boolean registerHandler(final EventHandler handler, final BundleContext context) {
    boolean _xblockexpression = false;
    {
      Hashtable<String,Object> _hashtable = new Hashtable<String,Object>();
      final Dictionary<String,Object> ht = _hashtable;
      final String[] topics = { ErlideEventTracer.ERLIDE_EVENT_TOPIC };
      ht.put(EventConstants.EVENT_TOPIC, topics);
      String _name = EventHandler.class.getName();
      context.registerService(_name, handler, ht);
      boolean _hasHandlers = ErlideEventTracer.hasHandlers = true;
      _xblockexpression = (_hasHandlers);
    }
    return _xblockexpression;
  }
}
