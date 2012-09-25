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
    var nodes = _.uniq(_.map(counters, function (c) {
        if (c["node"] instanceof Array) {
            return c['node'].join(",");
        }
        return c['node']
    }));

    var ids = _.uniq(_.map(counters, id));

    var headers = _.map(nodes, function (node) {
        return "<th>" + node + "</th>";
    });
    var header_str = "<tr><th></th>" + headers + "</tr>";
    $("#counters thead:last").append(header_str);

    var grouped = _.groupBy(counters, 'key');
    var keys = _.keys(grouped).sort();

    _.each(keys, function (key) {
        var counter_tds = _.map(nodes, function (n) {
            var d = _.find(grouped[key], function (c) { return c['node'] == n });
            if(d) {
                return "<td>" + d['rate'] + "</td>";
            } else {
                return "<td></td>";
            }
        });

        $("#counters tbody:last").append("<tr><td>" + key + "</td>" +
                                         counter_tds +
                                         "</tr>");
    });
}


function display_gauges(gauges) {
    var nodes = _.uniq(_.map(gauges, function (c) {
        if (c["node"] instanceof Array) {
            return c['node'].join(",");
        }
        return c['node']
    }));

    var headers = _.map(nodes, function (node) {
        return "<th>" + node + "</th>";
    });
    var header_str = "<tr><th>Node</th>" + headers + "</tr>";
    $("#gauges thead:last").append(header_str);

    var grouped = _.groupBy(gauges, 'key');
    var keys = _.keys(grouped).sort();

    _.each(keys, function (key) {
        var gauges_tds = _.map(nodes, function (n) {
            var d = _.find(grouped[key], function (c) { return c['node'] == n });
            if(d) {
                return "<td>" + d['value'] + "</td>";
            } else {
                return "<td></td>";
            }
        });

        $("#gauges tbody:last").append("<tr><td>" + key + "</td>" +
                                       gauges_tds +
                                       "</tr>");
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

    var nodes = _.uniq(_.pluck(node_histograms, 'node')).sort();


    _.each(nodes, function (node) {
        var histograms =
            _.groupBy(
                _.filter(node_histograms,
                         function (h) {return h['node'] == node }),
                function (h) { return h['id'] });

        _.each(histograms, function (histograms, i) {
            $("#node_histograms tbody:last").append("<tr>" +
                                                    "<td colspan='9'>" +
                                                    node + ": " + i + "</td>" +
                                                    "</tr>");

            _.each(histograms, function (h) {
                $("#node_histograms tbody:last").append(
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
    });
}




function format_us(us) {
    return (us / 1000).toFixed(4);
}