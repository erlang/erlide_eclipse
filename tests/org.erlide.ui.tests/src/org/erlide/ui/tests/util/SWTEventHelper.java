package org.erlide.ui.tests.util;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Touch;
import org.eclipse.swt.widgets.Widget;

public class SWTEventHelper {

    private final Event event;

    private SWTEventHelper(final int eventType) {
        event = new Event();
        event.type = eventType;
    }

    public static SWTEventHelper trigger(final int eventType) {
        return new SWTEventHelper(eventType);
    }

    public SWTEventHelper atX(final int x) {
        event.x = x;
        return this;
    }

    public SWTEventHelper atY(final int y) {
        event.y = y;
        return this;
    }

    public SWTEventHelper at(final int x, final int y) {
        return atX(x).atY(y);
    }

    public SWTEventHelper withCount(final int count) {
        event.count = count;
        return this;
    }

    public SWTEventHelper withStateMask(final int stateMask) {
        event.stateMask = stateMask;
        return this;
    }

    public SWTEventHelper withButton(final int button) {
        event.button = button;
        return this;
    }

    public SWTEventHelper withCharacter(final char character) {
        event.character = character;
        return this;
    }

    public SWTEventHelper withData(final Object data) {
        event.data = data;
        return this;
    }

    public SWTEventHelper withDetail(final int detail) {
        event.detail = detail;
        return this;
    }

    public SWTEventHelper withEnd(final int end) {
        event.end = end;
        return this;
    }

    public SWTEventHelper withStart(final int start) {
        event.start = start;
        return this;
    }

    public SWTEventHelper withRange(final int start, final int end) {
        return withStart(start).withEnd(end);
    }

    public SWTEventHelper withGC(final GC gc) {
        event.gc = gc;
        return this;
    }

    public SWTEventHelper withWidth(final int width) {
        event.width = width;
        return this;
    }

    public SWTEventHelper withHeight(final int height) {
        event.height = height;
        return this;
    }

    public SWTEventHelper withSize(final int width, final int height) {
        return withWidth(width).withHeight(height);
    }

    public SWTEventHelper withIndex(final int index) {
        event.index = index;
        return this;
    }

    public SWTEventHelper withItem(final Widget item) {
        event.item = item;
        return this;
    }

    public SWTEventHelper withKeyCode(final int keyCode) {
        event.keyCode = keyCode;
        return this;
    }

    public SWTEventHelper withKeyLocation(final int keyLocation) {
        event.keyLocation = keyLocation;
        return this;
    }

    public SWTEventHelper withMagnification(final double magnification) {
        event.magnification = magnification;
        return this;
    }

    public SWTEventHelper withRotation(final double rotation) {
        event.rotation = rotation;
        return this;
    }

    public SWTEventHelper withText(final String text) {
        event.text = text;
        return this;
    }

    public SWTEventHelper withTime(final int time) {
        event.time = time;
        return this;
    }

    public SWTEventHelper withTouches(final Touch[] touches) {
        event.touches = touches;
        return this;
    }

    public SWTEventHelper withXDirection(final int xDirection) {
        event.xDirection = xDirection;
        return this;
    }

    public SWTEventHelper withYDirection(final int yDirection) {
        event.yDirection = yDirection;
        return this;
    }

    public void on(final Widget widget) {
        event.widget = widget;
        event.display = widget.getDisplay();
        widget.notifyListeners(event.type, event);
    }
}
