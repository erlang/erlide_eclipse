package org.erlide.ui.tests.util;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Touch;
import org.eclipse.swt.widgets.Widget;

public class SWTEventHelper {

  private final Event event;

  private SWTEventHelper( int eventType ) {
    event = new Event();
    event.type = eventType;
  }

  public static SWTEventHelper trigger( int eventType ) {
    return new SWTEventHelper( eventType );
  }

  public SWTEventHelper atX( int x ) {
    event.x = x;
    return this;
  }

  public SWTEventHelper atY( int y ) {
    event.y = y;
    return this;
  }

  public SWTEventHelper at( int x, int y ) {
    return atX( x ).atY( y );
  }

  public SWTEventHelper withCount( int count ) {
    event.count = count;
    return this;
  }

  public SWTEventHelper withStateMask( int stateMask ) {
    event.stateMask = stateMask;
    return this;
  }

  public SWTEventHelper withButton( int button ) {
    event.button = button;
    return this;
  }

  public SWTEventHelper withCharacter( char character ) {
    event.character = character;
    return this;
  }

  public SWTEventHelper withData( Object data ) {
    event.data = data;
    return this;
  }

  public SWTEventHelper withDetail( int detail ) {
    event.detail = detail;
    return this;
  }

  public SWTEventHelper withEnd( int end ) {
    event.end = end;
    return this;
  }

  public SWTEventHelper withStart( int start ) {
    event.start = start;
    return this;
  }

  public SWTEventHelper withRange( int start, int end ) {
    return withStart( start ).withEnd( end );
  }

  public SWTEventHelper withGC( GC gc ) {
    event.gc = gc;
    return this;
  }

  public SWTEventHelper withWidth( int width ) {
    event.width = width;
    return this;
  }

  public SWTEventHelper withHeight( int height ) {
    event.height = height;
    return this;
  }

  public SWTEventHelper withSize( int width, int height ) {
    return withWidth( width ).withHeight( height );
  }

  public SWTEventHelper withIndex( int index ) {
    event.index = index;
    return this;
  }

  public SWTEventHelper withItem( Widget item ) {
    event.item = item;
    return this;
  }

  public SWTEventHelper withKeyCode( int keyCode ) {
    event.keyCode = keyCode;
    return this;
  }

  public SWTEventHelper withKeyLocation( int keyLocation ) {
    event.keyLocation = keyLocation;
    return this;
  }

  public SWTEventHelper withMagnification( double magnification ) {
    event.magnification = magnification;
    return this;
  }

  public SWTEventHelper withRotation( double rotation ) {
    event.rotation = rotation;
    return this;
  }

  public SWTEventHelper withText( String text ) {
    event.text = text;
    return this;
  }

  public SWTEventHelper withTime( int time ) {
    event.time = time;
    return this;
  }

  public SWTEventHelper withTouches( Touch[] touches ) {
    event.touches = touches;
    return this;
  }

  public SWTEventHelper withXDirection( int xDirection ) {
    event.xDirection = xDirection;
    return this;
  }

  public SWTEventHelper withYDirection( int yDirection ) {
    event.yDirection = yDirection;
    return this;
  }

  public void on( Widget widget ) {
    event.widget = widget;
    event.display = widget.getDisplay();
    widget.notifyListeners( event.type, event );
  }
}
