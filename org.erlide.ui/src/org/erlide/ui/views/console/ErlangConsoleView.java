package org.erlide.ui.views.console;

/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.ErlBackend;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.ErlideBackend;
import org.erlide.runtime.backend.IShell;
import org.erlide.runtime.backend.console.ErlConsoleModel;
import org.erlide.runtime.backend.console.ErlConsoleModelListener;
import org.erlide.runtime.backend.console.IoRequest;
import org.erlide.runtime.backend.console.ErlConsoleModel.ConsoleEventHandler;
import org.erlide.runtime.debug.ErlangProcess;
import org.erlide.ui.views.BackendContentProvider;
import org.erlide.ui.views.BackendLabelProvider;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlangConsoleView extends ViewPart implements
		ErlConsoleModelListener {

	public static final String ID = "org.erlide.ui.views.console";

	private static final Color[] colors = {
			new Color(Display.getDefault(), 0xFF, 0xFF, 0xFF),
			new Color(Display.getDefault(), 0xCC, 0xFF, 0xFF),
			new Color(Display.getDefault(), 0xFF, 0xCC, 0xFF),
			new Color(Display.getDefault(), 0xFF, 0xFF, 0xCC),
			new Color(Display.getDefault(), 0xCC, 0xCC, 0xFF),
			new Color(Display.getDefault(), 0xCC, 0xFF, 0xCC),
			new Color(Display.getDefault(), 0xFF, 0xCC, 0xCC),
			new Color(Display.getDefault(), 0x99, 0xFF, 0xFF),
			new Color(Display.getDefault(), 0xFF, 0x99, 0xFF),
			new Color(Display.getDefault(), 0xFF, 0xFF, 0x99),
			new Color(Display.getDefault(), 0x99, 0xCC, 0xFF),
			new Color(Display.getDefault(), 0xCC, 0x99, 0xFF),
			new Color(Display.getDefault(), 0xFF, 0x99, 0xCC),
			new Color(Display.getDefault(), 0xFF, 0xCC, 0x99),
			new Color(Display.getDefault(), 0x99, 0xFF, 0xCC),
			new Color(Display.getDefault(), 0xCC, 0xFF, 0x99),
			new Color(Display.getDefault(), 0x99, 0x99, 0xFF),
			new Color(Display.getDefault(), 0xFF, 0x99, 0x99),
			new Color(Display.getDefault(), 0x99, 0xFF, 0x99) };

	StyledText consoleText;
	boolean fGroupByLeader;
	boolean fColored;
	final Set<OtpErlangPid> pids = new TreeSet<OtpErlangPid>();
	ErlConsoleDocument fDoc;
	final List<String> history = new ArrayList<String>(10);
	StyledText consoleInput;
	SourceViewer consoleOutputViewer;
	SourceViewer consoleInputViewer;
	ErlConsoleModel model;
	IShell shell;
	ErlideBackend backend;
	private Action action;

	private ComboViewer backends;

	public ErlangConsoleView() {
		super();
		model = new ErlConsoleModel();
		final ConsoleEventHandler handler = model.getHandler();
		backend = ErlangCore.getBackendManager().getIdeBackend();
		try {
			final Job j = new Job("shell opener") {
				@Override
				protected IStatus run(final IProgressMonitor monitor) {
					if (backend == null) {
						schedule(400);
					} else {
						backend.getEventDaemon().addListener(handler);
						shell = backend.getShellManager().openShell("main");
					}
					return Status.OK_STATUS;
				}
			};
			j.setSystem(true);
			j.setPriority(Job.SHORT);
			j.schedule(400);
		} catch (final Exception e) {
			ErlLogger.warn(e);
		}
		model.addListener(this);
		fDoc = new ErlConsoleDocument(model);
	}

	@Override
	public void dispose() {
		backend.getEventDaemon().removeListener(model.getHandler());
		model.dispose();
		super.dispose();
	}

	@Override
	public void createPartControl(final Composite parent) {
		final Composite container = parent;
		container.setLayout(new GridLayout(2, false));

		final Label label = new Label(container, SWT.SHADOW_NONE);
		label.setText("Erlang backend node");
		backends = new ComboViewer(container, SWT.SINGLE | SWT.V_SCROLL);
		final Combo combo = backends.getCombo();
		combo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1,
				1));
		backends.setContentProvider(new BackendContentProvider());
		backends.setLabelProvider(new BackendLabelProvider());
		backends.setInput(ErlangCore.getBackendManager());
		backends.setSelection(new StructuredSelection(backend));

		consoleOutputViewer = new SourceViewer(container, null, SWT.V_SCROLL
				| SWT.H_SCROLL | SWT.MULTI | SWT.READ_ONLY | SWT.BORDER);
		consoleOutputViewer.setDocument(fDoc);
		consoleText = (StyledText) consoleOutputViewer.getControl();
		consoleText.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, true,
				2, 1));

		consoleOutputViewer
				.configure(new ErlangConsoleSourceViewerConfiguration());

		consoleText.setFont(JFaceResources.getTextFont());
		consoleText.setEditable(false);
		consoleText.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDown(final MouseEvent e) {
				try {
					final int ofs = consoleText.getOffsetAtLocation(new Point(
							e.x, e.y));
					final IoRequest req = model.findAtPos(ofs);
					clearMarks();
					if (req.getSender() != null) {
						final List<IoRequest> reqs = model.getAllFrom(req
								.getSender());
						markRequests(reqs);
					}
				} catch (final Exception ex) {
				}
			}
		});
		consoleText.addMouseTrackListener(new MouseTrackAdapter() {
			@Override
			public void mouseHover(final MouseEvent e) {
				try {
					final int ofs = consoleText.getOffsetAtLocation(new Point(
							e.x, e.y));
					final IoRequest req = model.findAtPos(ofs);
					consoleText.setToolTipText("sent by "
							+ ErlangProcess.toLocalPid(req.getSender()));
				} catch (final Exception ex) {
				}
			}
		});
		consoleText.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(final KeyEvent e) {
				final boolean isHistoryCommand = ((e.stateMask & SWT.CTRL) == SWT.CTRL)
						&& ((e.keyCode == SWT.ARROW_UP) || (e.keyCode == SWT.ARROW_DOWN));
				if ((e.character != (char) 0) || isHistoryCommand) {
					createInputField(e.character);
					e.doit = false;
				}
			}
		});
		initializeToolBar();
	}

	void createInputField(final char first) {
		if (first == SWT.ESC) {
			return;
		}
		consoleText.setSelection(consoleText.getCharCount());
		final Rectangle rect = consoleText.getClientArea();
		final Point relpos = consoleText.getLocationAtOffset(consoleText
				.getCharCount());

		final Shell container = new Shell(consoleText.getShell(), SWT.MODELESS);
		container.setLayout(new FillLayout());
		consoleInputViewer = new SourceViewer(container, null, SWT.MULTI
				| SWT.WRAP | SWT.V_SCROLL);
		consoleInputViewer.setDocument(new Document());
		consoleInputViewer
				.configure(new ErlangConsoleSourceViewerConfiguration());
		consoleInput = (StyledText) consoleInputViewer.getControl();
		consoleInput.setParent(container);
		container.setAlpha(220);

		final int b = 1;
		final Point screenPos = consoleText.toDisplay(relpos.x - b, relpos.y
				- b);
		container.setLocation(screenPos);
		container.setSize(rect.width - relpos.x, rect.height - relpos.y);

		consoleInput.addKeyListener(new KeyAdapter() {
			int navIndex = history.size();

			@Override
			public void keyPressed(final KeyEvent e) {
				final boolean historyMode = (e.stateMask & SWT.CTRL) == SWT.CTRL;
				if (e.keyCode == 13 && isInputComplete()) {
					sendInput();
					container.close();
					e.doit = false;
				} else if (e.keyCode == 13) {
					final Rectangle loc = container.getBounds();
					final int topIndex = consoleInput.getTopIndex();
					final int lineCount = consoleInput.getLineCount();
					final int lineHeight = consoleInput.getLineHeight();
					final int visibleLines = loc.height / lineHeight;
					final int maxLines = consoleText.getSize().y / lineHeight
							- 1;
					if (topIndex + visibleLines - 1 <= lineCount
							&& visibleLines < maxLines) {
						container.setBounds(loc.x, loc.y - lineHeight,
								loc.width, loc.height + lineHeight);
						consoleInput.setTopIndex(lineCount - visibleLines + 1);
					}
				} else if (historyMode && e.keyCode == SWT.ARROW_UP) {
					if (navIndex > 0) {
						navIndex--;
					}
					final String s = (history.size() > navIndex) ? history
							.get(navIndex) : "";
					consoleInput.setText(s);
					consoleInput.setSelection(consoleInput.getText().length());
					fixPosition(container);
				} else if (historyMode && e.keyCode == SWT.ARROW_DOWN) {
					if (navIndex < history.size()) {
						navIndex++;
					}
					final String s = navIndex < history.size() ? history
							.get(navIndex) : "";
					consoleInput.setText(s);
					consoleInput.setSelection(consoleInput.getText().length());
					fixPosition(container);
				} else if (e.keyCode == SWT.ESC) {
					container.close();
				}
			}

		});
		consoleInput.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(final FocusEvent e) {
				// container.close();
			}
		});
		consoleInput.setFont(consoleText.getFont());
		consoleInput.setBackground(consoleText.getBackground());
		consoleInput.setWordWrap(true);

		if (first != 0) {
			consoleInput.setText("" + first);
		} else {
			final String s = history.size() > 0 ? history.get(0) : "";
			consoleInput.setText(s);
			fixPosition(container);
		}
		consoleInput.setSelection(consoleInput.getCharCount());

		container.setVisible(true);
		consoleInput.setFocus();
	}

	void fixPosition(final Shell container) {
		final Rectangle loc = container.getBounds();
		final int lineCount = consoleInput.getLineCount();
		final int lineHeight = consoleInput.getLineHeight();
		final int visibleLines = loc.height / lineHeight;
		final int maxLines = consoleText.getSize().y / lineHeight - 1;
		final int lines = Math.max(
				Math.min(maxLines, lineCount) - visibleLines, visibleLines);
		if (visibleLines - 1 <= lineCount) {
			container.setBounds(loc.x, loc.y - lineHeight * lines, loc.width,
					loc.height + lineHeight * lines);
		}
	}

	boolean isInputComplete() {
		try {
			final String str = consoleInput.getText();
			final OtpErlangObject o = ErlBackend.parseString(ErlangCore
					.getBackendManager().getIdeBackend(), str);
			if (o instanceof OtpErlangList && ((OtpErlangList) o).arity() == 0) {
				return false;
			}
		} catch (final BackendException e) {
			return false;
		}
		return true;
	}

	protected void sendInput() {
		final String s = consoleInput.getText();
		input(s);
		consoleInput.setText("");
		refreshView();
	}

	private void updateConsoleView() {
		consoleText.setRedraw(false);
		consoleText.setText("");
		for (final IoRequest req : model.getContentList()) {
			consoleText.append(req.getMessage());
			if (fColored) {
				markRequest(req);
			}
		}
		consoleText.setRedraw(true);
		consoleText.setSelection(consoleText.getCharCount());
	}

	Color getColor(final OtpErlangPid sender) {
		int ix = 0;
		for (final Object element : pids) {
			final OtpErlangPid pid = (OtpErlangPid) element;
			if (pid.equals(sender)) {
				break;
			}
			ix++;
		}
		if (ix < colors.length - 1) {
			return colors[ix % 19 + 1];
		}
		return colors[0];
	}

	public void input(final String data) {
		model.input(data);
		shell.send(data);
		addToHistory(data.trim());
	}

	void refreshView() {
		if (consoleText.isDisposed()) {
			return;
		}
		try {
			updateConsoleView();
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	public void addToHistory(final String in) {
		if (history.indexOf(in) != -1) {
			history.remove(in);
		}
		history.add(in);
		if (history.size() > 50) {
			history.remove(0);
		}
	}

	public List<String> getHistory() {
		return history;
	}

	public void markRequests(final List<IoRequest> reqs) {
		for (final Object element0 : reqs) {
			final IoRequest element = (IoRequest) element0;
			markRequest(element);
		}
	}

	public void markRequest(final IoRequest req) {
		final StyleRange range = new StyleRange();
		range.start = req.getStart();
		range.length = req.getLength();
		range.background = getColor(fGroupByLeader ? req.getLeader() : req
				.getSender());
		consoleText.setStyleRange(range);
	}

	public void clearMarks() {
		final StyleRange range = new StyleRange();
		range.start = 0;
		range.length = consoleText.getCharCount();
		consoleText.setStyleRange(range);
	}

	public void setInput(final String str) {
		consoleInput.setText(str);
		consoleInput.setSelection(str.length());
	}

	@Override
	public void setFocus() {
	}

	private void initializeToolBar() {
		final IActionBars bars = getViewSite().getActionBars();
		final IToolBarManager toolBarManager = bars.getToolBarManager();
		{
			action = new Action("New Action") {
				@Override
				public int getStyle() {
					return AS_DROP_DOWN_MENU;
				}
			};
			action.setText("Backends");
			action.setToolTipText("backend list");
			action.setImageDescriptor(PlatformUI.getWorkbench()
					.getSharedImages().getImageDescriptor(
							ISharedImages.IMG_OBJS_INFO_TSK));
			toolBarManager.add(action);
		}

		final IMenuManager menuManager = bars.getMenuManager();
		menuManager.add(action);

	}

	public void changed(final ErlConsoleModel erlConsoleModel) {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				refreshView();
			}
		});
	}

}
