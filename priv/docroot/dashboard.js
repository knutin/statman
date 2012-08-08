var nodes = {};


$(document).ready(function() {
    var e = new EventSource("statman/stream");
    e.addEventListener("open", function (event) {
    });

    e.onmessage = function (event) {
        var data = $.parseJSON(event.data);
        //console.log(data);

        $("#counters thead").html('');
        $("#counters tbody").html('');
        $("#gauges thead").html('');
        $("#gauges tbody").html('');
        $("#histograms tbody").html('');
        $("#node_histograms tbody").html('');

        display_rates(by_type(data, 'counter'));
        display_gauges(by_type(data, 'gauge'));
        display_histograms(by_type(data, 'histogram'));
        display_node_histograms(by_type(data, 'histogram'));
    };
});

function by_type(d, type) {
    return _.filter(d['metrics'], function (m) { return m['type'] == type});
}

function id(c) {
    if (c['id']) {
        return c['id'] + ":" + c['key'];
    } else {
        return c['key'];
    }
}


function display_rates(counters) {
    var ids = _.uniq(_.map(counters, id));

    var headers = _.map(ids, function (id) {
        return "<th>" + id + "</th>";
    });
    var header_str = "<tr><th>Node</th>" + headers + "</tr>";
    $("#counters thead:last").append(header_str);


    var nodes = _.groupBy(counters, function (c) { return c['node'] });

    _.each(nodes, function (counters, node) {
        var counter_tds = _.map(ids, function (i) {
            var d = _.find(counters, function (c) { return id(c) == i });
            if(d) {
                return "<td>" + d['rate'] + "</td>";
            } else {
                return "<td></td>";
            }
        });

        $("#counters tbody:last").append("<tr><td>" + node + "</td>" +
                                         counter_tds +
                                         "</tr>");
    });
}


function display_gauges(gauges) {
    var ids = _.uniq(_.map(gauges, id));

    var headers = _.map(ids, function (id) {
        return "<th>" + id + "</th>";
    });
    var header_str = "<tr><th>Node</th>" + headers + "</tr>";
    $("#gauges thead:last").append(header_str);

    var nodes = _.groupBy(gauges, function (c) { return c['node'] });

    _.each(nodes, function (gauges, node) {
        var gauges_tds = _.map(ids, function (i) {
            var d = _.find(gauges, function (c) { return id(c) == i });
            if(d) {
                return "<td>" + d['value'] + "</td>";
            } else {
                return "<td></td>";
            }
        });

        $("#gauges tbody:last").append("<tr><td>" + node + "</td>" +
                                       gauges_tds +
                                       "</tr>");
    });

    return;

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


function display_histograms(all_histograms) {
    var merged_histograms = _.filter(all_histograms, function (h) {
        return h["node"] instanceof Array
    });
    var histograms = _.groupBy(merged_histograms, function (h) { return h['id'] });

    _.each(histograms, function (histograms, i) {
        $("#histograms tbody:last").append("<tr>" +
                                           "<td colspan='9'>" +
                                           histograms[0]['id'] + "</td>" +
                                           "</tr>");

        _.each(histograms, function (h) {
            $("#histograms tbody:last").append(
                "<tr>" +
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
    });
}

function display_node_histograms(all_histograms) {
    var node_histograms = _.reject(all_histograms, function (h) {
        return h["node"] instanceof Array
    });

    var histograms = _.groupBy(node_histograms, function (h) { return h['node'] });

    _.each(histograms, function (histograms, node) {
        $("#node_histograms tbody:last").append("<tr>" +
                                                "<td colspan='9'>" +
                                                node + "</td>" +
                                                "</tr>");

        _.each(histograms, function (h) {
            $("#node_histograms tbody:last").append(
                "<tr>" +
                    "<td></td>" +
                    "<td>" + id(h) + "</td>" +
                    "<td>" + h['rate'] + "</td>" +
                    "<td>" + h['observations'] + "</td>" +
                    "<td>" + format_us(h['mean']) + " ms</td>" +
                    "<td>" + format_us(h['sd']) + "</td>" +
                    "<td>" + format_us(h['p95']) + " ms</td>" +
                    "<td>" + format_us(h['p99']) + " ms</td>" +
                    "<td>" + format_us(h['max']) + " ms</td>" +
                    "</tr>");
        });
    });
}




function format_us(us) {
    return (us / 1000).toFixed(4);
}