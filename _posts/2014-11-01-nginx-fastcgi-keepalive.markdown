---
layout: post
title:  "如何保持Nginx与FastCGI的连接（Keepalive）"
date:   2014-11-01 21:51:00
---


用ApacheBench做压力测试时，发现压力过程中会出现很多TIME_WAIT的连接。这些连接的源端是FastCGI server的9000端口，因为在当时的环境下只有部署在同一台机器上的Nginx会连接这个端口，所以目的端应该是nginx。

根据TCP的四次挥手过程推断是FastCGI端主动关闭连接所致。看了一下所使用的C++ FastCGI库的源代码，果然印证了这个猜想。在client发起的请求未设置keepConnection标志时，FastCGI的代码会主动关闭掉这个连接。

{% highlight c %}
void FCGX_Finish_r(FCGX_Request *reqDataPtr)
{
    int close;

    if (reqDataPtr == NULL) {
        return;
    }

    close = !reqDataPtr->keepConnection;

    /* This should probably use a 'status' member instead of 'in' */
    if (reqDataPtr->in) {
        close |= FCGX_FClose(reqDataPtr->err);
        close |= FCGX_FClose(reqDataPtr->out);

        close |= FCGX_GetError(reqDataPtr->in);
    }

    FCGX_Free(reqDataPtr, close);
}

void FCGX_Free(FCGX_Request * request, int close)
{
    if (request == NULL)
        return;

    FCGX_FreeStream(&request->in);
    FCGX_FreeStream(&request->out);
    FCGX_FreeStream(&request->err);
    FreeParams(&request->paramsPtr);

    if (close) {
        OS_IpcClose(request->ipcFd);
        request->ipcFd = -1;
    }
}
{% endhighlight %}

观察当一个请求到来时，nginx与FastCGI之间的网络数据传输时序，我们发现每个连接结束的时候都是127.0.0.1:9000这边主动发起FIN的，如下面倒数第3行所示。

    $ sudo tcpdump -nn -i lo 'port 9000'
    tcpdump: verbose output suppressed, use -v or -vv for full protocol decode
    listening on lo, link-type EN10MB (Ethernet), capture size 96 bytes
    10:10:04.934015 IP 127.0.0.1.60864 > 127.0.0.1.9000: S 133391565:133391565(0) win 32792 <mss 16396,sackOK,timestamp 632337656 0,nop,wscale 9>
    10:10:04.934086 IP 127.0.0.1.9000 > 127.0.0.1.60864: S 1399737016:1399737016(0) ack 133391566 win 32768 <mss 16396,sackOK,timestamp 632337656 632337656,nop,wscale 9>
    10:10:04.934101 IP 127.0.0.1.60864 > 127.0.0.1.9000: . ack 1 win 65 <nop,nop,timestamp 632337656 632337656>
    10:10:04.934033 IP 127.0.0.1.60864 > 127.0.0.1.9000: P 1:1249(1248) ack 1 win 65 <nop,nop,timestamp 632337656 632337656>
    10:10:04.934043 IP 127.0.0.1.9000 > 127.0.0.1.60864: . ack 1249 win 69 <nop,nop,timestamp 632337656 632337656>
    10:10:04.935216 IP 127.0.0.1.9000 > 127.0.0.1.60864: P 1:289(288) ack 1249 win 69 <nop,nop,timestamp 632337657 632337656>
    10:10:04.935224 IP 127.0.0.1.60864 > 127.0.0.1.9000: . ack 289 win 67 <nop,nop,timestamp 632337657 632337657>
    10:10:04.935243 IP 127.0.0.1.9000 > 127.0.0.1.60864: F 289:289(0) ack 1249 win 69 <nop,nop,timestamp 632337657 632337657>
    10:10:04.935317 IP 127.0.0.1.60864 > 127.0.0.1.9000: F 1249:1249(0) ack 290 win 67 <nop,nop,timestamp 632337657 632337657>
    10:10:04.935329 IP 127.0.0.1.9000 > 127.0.0.1.60864: . ack 1250 win 69 <nop,nop,timestamp 632337657 632337657>


很多书上讲到让server端主动关闭不好，因为那会在server端累计很多TIME_WAIT的连接，占用大量socket资源。然而在我们的系统中，client也在同一台机器上，如果只是单纯地将关闭连接的动作交给client来做，仍然避免不了产生大量TIME_WAIT。

正确的解决办法是使用连接池并保持长连接。 长连接的保持需要client和server端同时配合，单方面的努力都是徒劳的。

首先要解决的问题是不要让FastCGI主动关闭。虽然FastCGI本身没有这样的觉悟，但是我们可以在发起的请求中设置keepConnection的标志位，让FastCGI处理完这个请求之后保持连接。我们的client是nginx（做了一辈子server，但在FastCGI面前还是得乖乖滴当client），必须想办法让nginx来设置这个标志位。好在nginx已经有一个叫做fastcgi_keep_conn的指令，只要将它设置为on，那么发出的请求中都会设置keepConnection位。


修改Nginx的配置文件之后，重新测试发现server端不再主动关闭连接。但是，连接却被client端主动关闭，如下面倒数第3行所示。不是说好的keepConnection吗？错了，fastcgi_keep_conn这个指令只是告诉FastCGI server，让他保持连接，但并client（也就是nginx）不会受此影响，仍然是一个请求一个连接。

    $ sudo tcpdump -nn -i lo 'port 9000'
    tcpdump: verbose output suppressed, use -v or -vv for full protocol decode
    listening on lo, link-type EN10MB (Ethernet), capture size 96 bytes
    10:40:49.901780 IP 127.0.0.1.33485 > 127.0.0.1.9000: S 2666831781:2666831781(0) win 32792 <mss 16396,sackOK,timestamp 634182624 0,nop,wscale 9>
    10:40:49.901870 IP 127.0.0.1.9000 > 127.0.0.1.33485: S 3383385539:3383385539(0) ack 2666831782 win 32768 <mss 16396,sackOK,timestamp 634182624 634182624,nop,wscale 9>
    10:40:49.901788 IP 127.0.0.1.33485 > 127.0.0.1.9000: . ack 1 win 65 <nop,nop,timestamp 634182624 634182624>
    10:40:49.901835 IP 127.0.0.1.33485 > 127.0.0.1.9000: P 1:1113(1112) ack 1 win 65 <nop,nop,timestamp 634182624 634182624>
    10:40:49.901851 IP 127.0.0.1.9000 > 127.0.0.1.33485: . ack 1113 win 69 <nop,nop,timestamp 634182624 634182624>
    10:40:49.902567 IP 127.0.0.1.9000 > 127.0.0.1.33485: P 1:161(160) ack 1113 win 69 <nop,nop,timestamp 634182625 634182624>
    10:40:49.902577 IP 127.0.0.1.33485 > 127.0.0.1.9000: . ack 161 win 67 <nop,nop,timestamp 634182625 634182625>
    10:40:49.902639 IP 127.0.0.1.33485 > 127.0.0.1.9000: F 1113:1113(0) ack 161 win 67 <nop,nop,timestamp 634182625 634182625>
    10:40:49.902659 IP 127.0.0.1.9000 > 127.0.0.1.33485: F 161:161(0) ack 1114 win 69 <nop,nop,timestamp 634182625 634182625>
    10:40:49.902666 IP 127.0.0.1.33485 > 127.0.0.1.9000: . ack 162 win 67 <nop,nop,timestamp 634182625 634182625>


前面已经提到，保持连接需要client和server端同时配合才能完成，下面看看怎么让client端复用已建立的连接。标准的作法是使用连接池，那怎么强制nginx下使用连接池呢？原来在nginx中还有另外一个配置指令叫做keepalive。这个指令设置后，nginx每次请求时都从一个连接池里借用一个已建立的连接。该指令跟一个整数参数，用来指定连接池中连接的数量。

再次修改nginx配置后运行同样的测试，观察发现连接只建立了一个，在该连接上先后出现了两次请求。注意下面的tcpdump输出中包含了两个请求，可以从时间的间隔上来区分它们。

    $ sudo tcpdump -nn -i lo 'port 9000'
    tcpdump: verbose output suppressed, use -v or -vv for full protocol decode
    listening on lo, link-type EN10MB (Ethernet), capture size 96 bytes
    13:42:50.038877 IP 127.0.0.1.36830 > 127.0.0.1.9000: S 3350400740:3350400740(0) win 32792 <mss 16396,sackOK,timestamp 645102761 0,nop,wscale 9>
    13:42:50.038891 IP 127.0.0.1.9000 > 127.0.0.1.36830: S 1993164669:1993164669(0) ack 3350400741 win 32768 <mss 16396,sackOK,timestamp 645102761 645102761,nop,wscale 9>
    13:42:50.038898 IP 127.0.0.1.36830 > 127.0.0.1.9000: . ack 1 win 65 <nop,nop,timestamp 645102761 645102761>
    13:42:50.038933 IP 127.0.0.1.36830 > 127.0.0.1.9000: P 1:1249(1248) ack 1 win 65 <nop,nop,timestamp 645102761 645102761>
    13:42:50.038938 IP 127.0.0.1.9000 > 127.0.0.1.36830: . ack 1249 win 69 <nop,nop,timestamp 645102761 645102761>
    13:42:50.040265 IP 127.0.0.1.9000 > 127.0.0.1.36830: P 1:289(288) ack 1249 win 69 <nop,nop,timestamp 645102762 645102761>
    13:42:50.040273 IP 127.0.0.1.36830 > 127.0.0.1.9000: . ack 289 win 67 <nop,nop,timestamp 645102762 645102762>
    13:42:53.583465 IP 127.0.0.1.36830 > 127.0.0.1.9000: P 1249:2497(1248) ack 289 win 67 <nop,nop,timestamp 645106306 645102762>
    13:42:53.583936 IP 127.0.0.1.9000 > 127.0.0.1.36830: P 289:577(288) ack 2497 win 74 <nop,nop,timestamp 645106306 645106306>
    13:42:53.583953 IP 127.0.0.1.36830 > 127.0.0.1.9000: . ack 577 win 69 <nop,nop,timestamp 645106306 645106306>
    13:42:56.943110 IP 127.0.0.1.36830 > 127.0.0.1.9000: P 2497:3745(1248) ack 577 win 69 <nop,nop,timestamp 645109665 645106306>
    13:42:56.943505 IP 127.0.0.1.9000 > 127.0.0.1.36830: P 577:865(288) ack 3745 win 79 <nop,nop,timestamp 645109666 645109665>
    13:42:56.943518 IP 127.0.0.1.36830 > 127.0.0.1.9000: . ack 865 win 71 <nop,nop,timestamp 645109666 645109666>

总结一下，nginx与FastCGI之间的连接最好保持长连接，这样才能避免产生大量TIME_WAIT。要做到这一点，需要在nginx的配置文件中配合使用`fastcgi_keep_conn`和`keepalive`这两个指令以达到目的。
