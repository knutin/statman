$(document).ready(function() {
    var e = new EventSource("statman/stream");
    e.addEventListener("open", function (event) {
    });

    e.onmessage = function (event) {
        var data = $.parseJSON(event.data);
        //console.log(data);

        $("#title").html("statman@" + data['hostname']);
        $("title").html("statman@" + data['hostname']);

        $("#counters tbody").html('');
        $("#gauges tbody").html('');
        $("#histograms tbody").html('');

        var counters = _.groupBy(data['rates'], function (c) { return c['id'] });
        var gauges = _.groupBy(data['gauges'], function (g) { return g['id'] });
        var histograms = _.groupBy(data['histograms'], function (h) { return h['id']; });

        _.each(counters, append_counter);
        _.each(gauges, append_gauge);
        _.each(histograms, append_histogram);

    };
});

function append_counter(counters) {
    $("#counters tbody:last").append("<tr>" +
                                     "<td colspan='3'>" + counters[0]['id'] + "</td>" +
                                     "</tr>");

    _.each(counters, function (c) {
        $("#counters tbody:last").append("<tr>" +
                                         "<td></td>" +
                                         "<td>" + c['key'] + "</td>" +
                                         "<td>" + c['rate'] + "</td>" +
                                         "</tr>");
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
    $("#histograms tbody:last").append("<tr>" +
                                       "<td colspan='8'>" + histograms[0]['id'] + "</td>" +
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