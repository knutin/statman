var nodes = {};


$(document).ready(function() {
    var e = new EventSource("statman/stream");
    e.addEventListener("open", function (event) {
    });

    e.onmessage = function (event) {
        var data = $.parseJSON(event.data);
        console.log(data);

        if (data['nodes']) {
            $("#counters thead").html('');
            $("#counters tbody").html('');
            $("#gauges thead").html('');
            $("#gauges tbody").html('');

            display_rates(data);
            display_gauges(data);
        }

        if (data['merge']) {
            console.log(data);
            $("#histograms tbody").html('');


            var histograms = _.groupBy(data['merge']['histograms'],
                                       function (h) { return h['id']; });
            $("#histogram_nodes").html("from " + data['merge']['nodes'].join(","));
            _.each(histograms, append_histogram);
        }
    };
});

function display_rates(data) {
    var rates = _.uniq(_.flatten(_.map(data['nodes'], function (v) {
        return _.map(v['node']['rates'], function (r) {
            if (r['id']) {
                return r['id'] + ":" + r['key'];
            } else {
                return r['key'];
            }
        });
    }), true));

    var headers = _.map(rates, function (r) {
        return "<th>" + r + "</th>";
    });
    var header_str = "<tr><th>Node</th>" + headers + "</tr>";
    $("#counters thead:last").append(header_str);

    _.each(data['nodes'], function (n) {
        var rates = _.map(n['node']['rates'], function (r) {
            return "<td>" + r['rate'] + "</td>";
        });
        $("#counters tbody:last").append("<tr><td>" + n['node']['name'] + "</td>" +
                                         rates +
                                         "</tr>"
                                        );
    });
}

function display_gauges(data) {
    var gauges = _.uniq(_.flatten(_.map(data['nodes'], function (v) {
        return _.map(v['node']['gauges'], function (g) {
            if (g['id']) {
                return g['id'] + ":" + g['key'];
            } else {
                return g['key'];
            }
        });
    }), true));

    var headers = _.map(gauges, function (g) {
        return "<th>" + g + "</th>";
    });
    var header_str = "<tr><th>Node</th>" + headers + "</tr>";
    $("#gauges thead:last").append(header_str);

    _.each(data['nodes'], function (n) {
        var gauges = _.map(n['node']['gauges'], function (g) {
            return "<td>" + g['value'] + "</td>";
        });
        $("#gauges tbody:last").append("<tr><td>" + n['node']['name'] + "</td>" +
                                       gauges +
                                       "</tr>"
                                        );
    });


}




function append_gauge(gauges) {
    $("#gauges tbody:last").append("<tr>" +
                                   "<td colspan='3'>" + gauges[0]['id'] + "</td>" +
                                   "</tr>");
    _.each(gauges, function (g) {
        $("#gauges tbody:last").append("<tr>" +
                                      "<td></td>" +
                                      "<td>" + g['key'] + "</td>" +
                                      "<td>" + g['value'] + "</td>" +
                                      "</tr>");
    });
}

function append_histogram(histograms) {
    console.log(histograms);
    $("#histograms tbody:last").append("<tr>" +
                                       "<td colspan='9'>" + histograms[0]['id'] + "</td>" +
                                       "</tr>");
    _.each(histograms, function (h) {
        $("#histograms tbody:last").append("<tr>" +
                                           "<td></td>" +
                                           "<td>" + h['key'] + "</td>" +
                                           "<td>" + h['rate'] + "</td>" +
                                           "<td>" + h['observations'] + "</td>" +
                                           "<td>" + format_us(h['mean']) + " ms</td>" +
                                           "<td>" + format_us(h['sd']) + "</td>" +
                                           "<td>" + format_us(h['p95']) + " ms</td>" +
                                           "<td>" + format_us(h['p99']) + " ms</td>" +
                                           "<td>" + format_us(h['max']) + " ms</td>" +
                                           "</tr>");
    });
}


function format_us(us) {
    return (us / 1000).toFixed(4);
}