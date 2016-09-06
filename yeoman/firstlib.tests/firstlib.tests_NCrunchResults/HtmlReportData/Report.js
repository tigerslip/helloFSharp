$(document).ready(function () {
    $(".resultPanel .result .resultHeader").click(function () {
        var parent = $(this).parent();

        if (parent.hasClass("collapsed")) {
            parent.removeClass("collapsed");
            parent.addClass("expanded");
        } else {
            parent.removeClass("expanded");
            parent.addClass("collapsed");
        }
    });

    function hideTreeNode(treeNode) {
        var children = $(".child" + treeNode.attr("id"));
        children.each(function () {
            hideTreeNode($(this));
        });
        treeNode.hide();

        if (treeNode.hasClass("expanded")) {
            treeNode.removeClass("expanded");
            treeNode.addClass("collapsed");
        }
    }

    $("#coveragePanel .childRow").click(function () {
        var children = $(".child" + $(this).attr("id"));
        var parent = $(this);

        if (parent.hasClass("collapsed")) {
            parent.removeClass("collapsed");
            parent.addClass("expanded");
            children.show();
        } else if (parent.hasClass("expanded")) {
            parent.removeClass("expanded");
            parent.addClass("collapsed");

            children.each(function() {
                hideTreeNode($(this));
            });
        }
    });

    $(".tab").click(function () {
        if ($(this).hasClass("selected") == false) {
            $(".tab").each(function() {
                $(this).addClass("unselected");
                $(this).removeClass("selected");
            });

            $(this).removeClass("unselected");
            $(this).addClass("selected");

            $(".resultPanel").hide();
        }
    });

    $("#buildResultsTab").click(function () {
        $("#buildResultsPanel").show();
    });

    $("#resultsTab").click(function () {
        $("#resultsPanel").show();
    });

    $("#coverageTab").click(function () {
        $("#coveragePanel").show();
    });

    function adjustTestResultVisibility(subClass, checkbox) {
        $("#resultsPanel .result .resultHeader." + subClass).each(function () {
            if (checkbox.checked)
                $(this).parent().show();
            else
                $(this).parent().hide();
        });
    }

    $('#showPassingTestsFilter input').change(function() { adjustTestResultVisibility("pass", this); });
    $('#showFailingTestsFilter input').change(function() { adjustTestResultVisibility("fail", this); });
    $('#showIgnoredTestsFilter input').change(function() { adjustTestResultVisibility("ignored", this); });
    $('#showNotRunTestsFilter input').change(function() { adjustTestResultVisibility("notRun", this); });
});
