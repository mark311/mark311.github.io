---
layout: post
title:  "在GitHub Pages上搭建我的个人博客"
date:   2014-10-12 00:07:54
categories: jekyll update
---
从事IT工作3年多了，曾几何时有过念头想搭建一个属于自己的博客，但始终没有克服掉拖延的毛病。最近学了很多新鲜的技术，越是接触得多，就越是发现有更多需要学。那问题就来了（可不是挖掘机哪家强），学习简单的东西则罢，稍微复杂一点的东西都需要在头脑中建立知识的结构，否则脑袋就会随之变得一团乱糟。我想，写技术博客是个不错学习新知识的方法，减轻我大脑记忆的负担。好记性不如烂笔头嘛~ 况且我记性也不好！

搭博客的方法有很多种，各有利弊。通常最简单的是在专门的博客网站上注册一个账号，譬如CSDN、博客圈等技术性论坛社区都允许用户建立自己的博客，作者登陆后在Web界面上管理博客。也可以买一台虚拟机，在上面搭个web服务器，然后选择一款博客系统，譬如wordpress之类的，这样对你的博客更有占用感，不过搭建起来麻烦。也见过有些人图省事，直接在github上开一个repository，用markdown来写博客，好处是用github可以呈现你的博文，git使得离线写博客成为现实，markdown天生的简单易懂易用，唯一不好的就是你的博文被嵌在github.com中，显得不那么专业。或者你还可以用APP engine，或者whatever你能把你的博文放上去的地方，总有一些地方不是很如我的意。

偶然的机会，看到这个叫做http://equation85.github.io 的博客，它的域名尽然是github.io，这让我很是好奇，于是发现了Github Pages+Jekyll这对好搭档。它有这些好处：

* 用Markdown撰写博文
* 帮你托管博文代码
* 允许你使用git管理
* 可以定制界面

好了，废话不多说了，现在来说说搭建Github Pages+Jekyll的过程吧。

首先你需要为博客建立一个github的repository，[github.io的首页](https://pages.github.com)会告诉你如何建立。不过需要注意的是：

1. Repository的名字必须是固定格式，每个github账号只能建立唯一的一个博客系统，对应的repository的名字是<name>.github.io，譬如我的就是mark311.github.io。
2. 初次提交之后，需要等很长一段时间才能生效，不过之后的改动就能立即生效了。

至此，你的网站搭建完毕。Github已为你安装了最新的Jekyll环境，所以你只要按照Jekyll的规范提交markdown博文就好了。Jekyll对整个根目录下的文件目录结构有要求，而且对post（博文）的文件名和部分文件内容的格式也有要求。 Jekyll提供了一个工具来生成整个目录结构，以及能在本地启动一个0.0.0.0:4000的供调试的web server。

Github Pages的帮助页面里有提到[如何在本地安装Jekyll](https://help.github.com/articles/using-jekyll-with-pages/)，对照着一步一步做就可以安装好Jekyll。不过在这里我遇到了一个坑。Jekyll是一个Ruby程序，帮助文档推荐使用bundler来安装，我完全按照它的建议一步步来做。但在安装bundler时，执行`gem install bundler`后一直没有返回，添加了-V参数来打开verbose——`gem install -V bundler`，结果程序卡在了请求rubygem.org的某个URL上，输出错误信息：`HTTP Response 302`。Google了这个问题，说要更新gem的版本。我本地的gem是2.0，而当前最新的是2.4，于是果断更新了，更新是通过下载gem的tarball，然后执行`ruby setup.rb`来安装的。

安装好Jekyll之后，在原来Gemfile的目录下执行这个命令来创建Jekyll的根目录结构：

    $ bundle exec jekyll new mark311.github.io

然后修改一下mark311.github.io/_config.yml，设置你的博客标题、描述等信息。OK之后进入到mark311.github.io目录，执行下面的命令来启动Jekyll的本地server：

    $ bundle exec jekyll serve

打开浏览器，访问localhost:4000。

完！
