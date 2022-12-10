(function () {
    if (typeof DrawMatrix === 'object') {
        return;
    }

    DrawMatrix = {
        render: function(containerId, data, settings) {
            let container = document.getElementById(containerId);
            container.innerHTML = "";
            let canvas = document.createElement("canvas");
            container.appendChild(canvas);

            const size = settings.size;
            canvas.width = settings.width;
            canvas.height = settings.height;

            var ctx = canvas.getContext("2d");
            ctx.clearRect(0, 0, canvas.width, canvas.height);

            for (point of data) {
                const [i, j, color] = point;
                ctx.fillStyle = color;
                ctx.fillRect(size * j, size * i, size, size);
            }
        }
    }
})();