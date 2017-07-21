package org.erlide.cover.ui.views.util

import org.erlide.cover.views.model.ICoverageObject

class ReportTemplate {

    ICoverageObject obj
    String date
    String type

    new(ICoverageObject obj, String date, String type) {
        this.obj = obj
        this.date = date
        this.type = type
    }

    def String getReport(boolean relative) {
        '''
            <html>
            <head>
                <style type="text/css">
                    «css»
                </style>
            </head>
            <body>
            <div class="title">Cover plugin for ErlIde - coverage report (generated at «date») </div>
            <br/>
            <h2>Coverage summary for: «obj.getLabel()» </h2>
            <table class>
            <tr>
                <th>Name</th>
                <th class="total">Total Lines</th>
                <th class="covered">Covered Lines</th>
                <th class="percentage">Coverage %</th>
            </tr>
            <tr>
                <td><b>«obj.getLabel()»</b></td>
                <td>«obj.getLinesCount()»</td>
                <td>«obj.getCoverCount()»</td>
                <td>«obj.getPercentageStringified()»</td>
            </tr>
            </table>
            <h2>Coverage breakdown by «type» </h2>
            <table>
            <tr>
                <th>Name</th>
                <th class="total">Total Lines</th>
                <th class="covered">Covered Lines</th>
                <th class="percentage">Coverage %</th>
            </tr>
              «FOR child : obj.children »
              <tr>
                <td><a href="«if(relative)child.relativePath else child.htmlPath»">«child.label»</a></td>
                <td>«child.getLinesCount()»</td>
                <td>«child.getCoverCount()»</td>
                <td>«child.getPercentageStringified()»</td>
              </tr>
              «ENDFOR»
            </table>
            <br/>
            <br/>
            <br/>
            <div class="footer">Cover plugin for ErlIde 0.2.0 &copy Erlang Solutions</div>
            </body>
            </html>
        '''
    }

    def getCss() {
        '''
            body {
                font-size: 12px:
            }

            h2 {
                font-size: 14px;
            }

            div.title {
                border: solid #333333 2px;
                font-size: 11px;
                color: #EEEEEE;
                text-align: center;
                background-color: #772222;
                padding: 4px;
            }

            div.footer {
                border: solid #333333 2px;
                font-size: 11px;
                color: #5599FF;
                text-align: center;
                background-color: #772222;
                padding: 3px;
            }

            table {
                width: 100%;
                border: solid #000000 1px;
                border-collapse: collapse;
                font-size: 12px;
                table-layout: fixed;
            }

            td {
                border: solid #000000 1px;
                padding: 2px 5px 2px 5px;
            }

            a, a:link {
                text-decoration: none;
                font-weight: bold;
                color: #1144CC;
            }

            a:visited {
                color: #112255;
            }

            a:hover {
                text-decoration: underline;
            }

            th {
                border: solid #000000 1px;
                background-color: #AA5555;
                color: #EEEEEE;
                padding: 3px;
            }

            th.total {
                width: 120px;
            }

            th.covered {
                width: 120px;
            }

            th.percentage {
                width: 160px;
            }
        '''
    }
}
