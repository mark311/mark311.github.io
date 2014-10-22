---
layout: post
title:  "翻译：最终一致性（Eventually Consistent）"
date:   2014-10-18 00:21:28
categories: 分布式 最终一致性
---
一年前我写过关于一致性模型的[第一版文章](http://www.allthingsdistributed.com/2007/12/eventually_consistent.html)。因为当时写的很匆忙，所以我并不是很满意。时至今日这个topic已经非常重要了，值得更缜密的对待。ACM Queue请我重新修订它，并将发布在他们的杂志上。所以我有这个机会来改进这篇文章。这就是那份新的版本。

最终一致性 - 在全球范围内建立可靠的分布式系统需要在一致性和可用性之间做权衡（trade-offs）。

亚马逊云计算的根基，是诸如S3（Simple Storage Service）、SimpleDB、EC2（Elastic Compute Cloud）等用以构建Internet规模级别计算平台的基础设施服务，以及上层一些丰富的应用。对这些基础设施服务的要求是非常严格的，必须在安全性、可伸缩性、可用性、性能和成本有效性方面达到足够的水准；并持续服务于全球数以百万计的客户。

这些服务的底层是大量分布式系统，运行在世界范围内。这种规模创造了额外的挑战。因为当一个系统处理万亿数量的请求时，通常情况下非常罕见的事件会必然出现。这需要在系统设计和架构上做出考虑。针对这些世界范围内的系统，我们广泛使用冗余（replication）技术来保证一致性性能（consistent performance）和高可用性。尽管“冗余”让我们离目标更近了，却不能用一种透明的方式完美的实现这些目标。在一些情况下，服务的使用者需要面对由于使用冗余技术带来的后果。

表现形式之一是所提供的数据一致性的类型，尤其是底层分布式系统提供一种数据冗余的最终一致性模型（One of the ways in which this manifests itself is in the type of data consistency that is provided, particularly when the underlying distributed system provides an eventual consistency model for data replication.）。在Amazon，我们设计这些大规模系统时使用一系列关于大规模数据冗余的指导原则和抽象方法，关注高可用性和数据一致性之间的平衡。在这篇文章里，我将介绍一些背景，它使得我们的方案能提供在全球范围内运行的可靠的分布式系统。早些发布在All Things Distributed网络博客上的[一篇文章](http://www.allthingsdistributed.com/2007/12/eventually_consistent.html)因为读者的帮助而得到了很大的改进。

### 历史视角

在理想世界里只有一种一致性模型：当更新发生时，所有观察者都能看到那个更新。这种模型第一次面临实现困难是发生在在70年代末的数据库系统中。在这个问题上最好的period piece是"Notes on Distributed Databases" by [Bruce Lindsay et al](http://acmqueue.com/modules.php?name=Content&pa=showpage&pid=233)[5]。 它奠定了数据库冗余的基础性原则，并讨论出来了一些达成数据一致性的技术方法。 许多这些技术方法尝试实现对分布式透明——也就是说，对于系统的用户来说，他们感觉仅有一个独立的系统，而不是多个相互协作的系统。在这个时期，许多系统更倾向于是整个系统失败，而不是打破这种透明[2]

在90年代中期，随着更大的Internet系统的出现，这些实践（practice）被重新提及。 在那个时候的人们开始认为可用性也许是这些系统最重要的属性，但他们还在挣扎着该用什么来换取（可用性）。 [Eric Brewer](http://www.cs.berkeley.edu/~brewer/), 加州伯克利大学的系统教授，同时也是Inktomi公司的领头人。他在2000年一个[keynote address to the PODC](http://www.cs.berkeley.edu/~brewer/cs262b-2004/PODC-keynote.pdf) (Principles of Distributed Computing) 会议[1]上提出了一种不同的trade-offs。他介绍了CAP定理，定理陈述了一个事实：系统共享数据的一致性、系统可用性和网络分区容错性这三个属性中，在任意时间仅有两个能满足。 更正式的证实可以在2002年Seth Gilbert and Nancy Lynch的一篇[论文](http://portal.acm.org/citation.cfm?doid=564585.564601)[4]中找到。

一个系统如果不能对网络分区容错，那么它是可以实现数据一致性和可用性的，实现的方法通常是使用事务协议（transaction protocols）。为此，客户端（client）和存储系统（storage system）必须是隶属于同一系统的部分，在某些场景下他们作为一个整体失败。照这点来看，客户端是不能观察到分区（partitions）的。一个重要的观察发现，在较大规模的分布式系统中，网络分区（network partitions）是一个不可回避的事实。因此，一致性和可用性不可能同时满足。这意味着被砍掉谁这个问题有两个选择：在可分区的前提下，放弃一致性使得系统可以保留系统的高可用性；而侧重一致性意味着在特定情况下系统是不可用的。

两种选项都要求客户端开发者意识到系统到底提供什么。如果系统强调一致性，开发者需要处理系统不可用这种事实，例如一个写操作。如果写操作因为系统不可用而失败，那么开发者必须决定接下来如何处理这些写失败的数据。如果系统强调可用性，那么写操作永远被接受，但是在某些情况下读操作将不能反映最近写入的结果。于是开发者要决定是否总需要访问绝对最新的数据。有一些应用程序能允许轻微的数据失鲜（stale），并且在这种模型下仍然能正确地服务。

原则上，事务系统中的[ACID](http://en.wikipedia.org/wiki/ACID) properties (atomicity, consistency, isolation, durability)所定义的一致性属性是一种完全不同的一致性保证。在ACID中，一致性是保证当一个事务完成时，数据库处在一致的状态。例如，当资金在两个账户之间的转移前后，两个账户总的金额不能改变。在基于ACID的系统中，这种一致性通常是开发者的责任，开发者必须编写支持事务的程序。不过，通常数据库会提供管理完整性限制。


### 一致性 —— 客户端和服务器

对于一致性存在两种视角。一种来自开发者或客户端（developer/client）：他们如何观察数据的变更。另一种来自服务器端：（数据）更新如何在系统中流动，和怎么保证更新能被给出去（what guarantees systems can give with respect to updates）

### 客户端一致性

客户端有这些组成部分：

* **一个存储系统.** 我们暂时把它当做一个黑匣子，但是必须假定底层的实现是一个大规模且高度分布的系统，用以保证持久性（durability）和可用性。

* **进程A.** 这是一个读写存储系统的进程。

* **进程B和C.** 这是两个独立于进程A的进程，它们也读写存储系统。他们是否是真实的进程或同一进程中的线程并不重要，重要的是他们是相互独立，且需要通信以共享信息。客户端一致性是关乎观察者（在这里指进程A, B或C）何时、以何种方式看到存储系统中数据对象的更新。在接下来的例子中会举例说明，进程A对数据对象产生更新后，不同种类的一致性。

* **强一致性（Strong consistency）.** 更新完成后，任何后续的访问都将返回更新后的值。

* **弱一致性（Weak consistency）.** 系统不保证后续访问会返回更新后的值，只有当若干条件满足之后才能返回。从更新发生后到所有观察者确定总能取到更新后的值的这段时间，称为不一致窗口（inconsistent window）。

* **最终一致性（Eventual consistency）.** 这是弱一致性的一个特殊形式。存储系统提供这样的保证：如果没有新的改变发生在数据对象上，最终所有的访问都将返回最后一次更新后的值。如果没有失败发生，不一致窗口的最大长度取决于通信延时、系统负载、冗余份数等因素。最流行的DNS（Domain Name System）系统就实现了最终一致性模型。根据一个配置模式（configured pattern）结合时间缓存控制，分布式地更新某个名字；最终所有client都会看到这个变更。

最终一致性模型有若干变体值得注意：

* **因果一致性（Causal consistency）.** 如果进程A已经把数据项被更新的消息告诉给了进程B，那么进程B后续的访问将返回更新后的结果，且新的写操作将确保替换掉先前的写操作结果。进程C与进程A之间并没有因果关系（估计作者是指没有进行更新消息的通信），那么进程C对数据的访问结果将取决于一般的最终一致性规则。

* **读你所写一致性（Read-your-writes consistency）.** 这是一种重要的模型。进程A更新一个数据项之后，再去访问它，总能得到更新后的值，并且不再会看到这个数据项更新之前的值。这是causal consistency模型的特殊形式。

* **会话一致性（Session consistency）.** 这是前中模型的实用版本（practical version）。进程在一个会话上下文中访问存储系统，只要会话未结束，系统提供read-your-writes一致性保证。如果会话因某种失败而意外结束，一个新的会话需要创建，但是一致性保证不会在两个会话之间重叠（the guarantees do not overlap the sessions）

* **单调读一致性（Monotonic read consistency）.** 如果进程已经看到了数据对象特定的值（猜想不一定是最新的值），那么任何后续的访问将不会返回任何先前更新的值。

* **单调写一致性（Monotonic write consistency）.** 系统保证写操作由同一个进程执行。编写不提供这种一致性级别保证的系统是众所周知的困难（Systems that do not guarantee this level of consistency are notoriously hard to program）。

上述属性可被组合。例如，monotonic reads可以和session-leve consistency相结合。从实用的角度来看，这两种属性（monotonic reads和read-your-writes）是最终一致性系统最可取的属性，但不是任何场合必须的。它们使得开发者构建应用变得更简单，同时允许存储系统放宽对一致性的要求，并提供高可用性。

就像你在这些变体中看到的，一些不同的场景可能出现，这取决于特殊的程序是否能正确处理因果关系。

最终一致性并不是极度（extreme）分布式系统中深奥难懂的属性。许多提供主备可靠性（primary-bakcup reliability）的现代RDBMSs同时实现同步和异步的冗余技术。在同步模式下，对冗余数据的同步更新也是事务（transaction）的一部分。在异步模式下，更新发生在被滞后的备份过程中，通常是通过log传递。如果主存储在log传递之前失败了，从晋级（promoted？）的备存储读数据将产生旧的、不一致的值。为了支持更具可伸缩的读取性能，RDBMSs已经开始提供从备存储上读数据的功能。这是一个提供最终一致性保证的经典案例，其中不一致窗口取决于周期性的log传递。


### 服务器端一致性

在服务器端，我们需要仔细看看数据更新的消息是如何在系统中流动的，以便理解是什么驱动（drive?）了不同的模式，这些模式将影响开发者使用系统的体验。在进一步开始之前，我们先来建立一些定义：

N = 存储冗余数据的节点数

W = 在更新操作完成之前，须确认收到的冗余数据的节点数

R = 使读操作完成所需联系的节点数

如果 W+R > N，那么写操作节点集合和读操作节点集合始终存在重叠，因而可以保证强一致性。在实现同步冗余技术的主备RDBMS的场景中：N=2, W=2, R=1，无论客户端从哪个节点读数据，始终都会得到一致的结果。在允许从备存储上读取数据的异步冗余的实现中：N=2, W=1, R=1，因为R+W=N，所以一致性无法得到保证。

这种配置是一种基本的仲裁协议（basic quorum protocols），它带来的问题是当系统不能成功完成W个节点的写操作时，这个写操作算作失败，标志着系统不可用。即例如，N=3, W=3但只有两个节点可用时，写操作将不得不以失败告终。

在高性能和高可用性的分布式存储系统中，冗余数据的份数通常大于2。关注容错的系统通常使用N=3 (W=2, R=2)的配置。提供大量读负载的系统，通常维持比容错系统还多的冗余数据份数；其N的值可能是数以十计或者百计，R为1，因此单个节点也能完成读操作。关注一致性的系统会令W=N，但这回降低写操作成功的概率。侧重容错而一致性不是重点的系统，通常令W=1以得到最小的更新延迟，并依赖延迟（蔓延）技术来更新其他的节点。

如何配置N, W和R取决于常用的操作是什么和那种性能路径需要被优化。在R=1且N=W的情况下，我们优化读操作；在W=1且R=N的情况下，我们优化写操作。当然在后一种情况中，出现故障会使得持久性（durability）得不到保证。假如W<(N+1)/2，在被写节点集合不相交的情况下，可能出现写入冲突。

弱/最终一致性出现在W+R<=N的情况下，意味着读写节点的集合可能不相交。如果这是一个深思熟虑，不考虑失败情况的配置，那么将R设置成其他任何值而非1将没有意义。这发生在两种常见的情况下：第一种提供大量的冗余备份以支持大规模的读操作，如前面提到的；第二种是当数据访问非常复杂。在一个简单的键值模型中，通过比较版本很容易找到最新写入的值，但是在单纯返回对象的系统中，很难决定最新的值是什么。大多数写集合数（W）小于冗余份数（N）的系统，会启用延迟写入更新数据到剩余冗余节点的机制。在冗余副本被更新到所有节点之前的一段时间，如前面提到的，我们称作不一致窗口。如果W+R<=N，系统在没有收到更新的节点上读数据将会是不准确的。

Read-your-writes、session和monotonic一致性是否能被实现总体上取决于客户端对服务端的“黏性”。如果每次都是同一个服务器，那么相对容易保证read-your-writes和monotonic reads一致性。虽然这使得负载均衡和容错变得稍微困难一点，但它是一个简单的方案。使用会话机制，因其本身具有黏性，使得这个机制变得更加显式（explicit），而且可以给客户端提供一个可参考的明确等级。

有时候客户端通过添加版本号来实现read-your-writes和monotonic reads，客户端抛弃掉比自己先前已读版本更早的数据。

分割（partitions）发生在系统中的节点无法达到其他节点时，但所有节点都能被客户端访问。如果你使用一个经典的多数仲裁法（majority quorum approach），那么拥有至少W个节点的分区能继续接受更新，而其他分区变得不可用。对于读操作也是同样的道理。假设两个集合会重叠，根据定义少数集合会变得不可用。分区不经常出现，但是的确会在数据中心之间出现，也会在数据中心内部出现。

在有些应用中，任何分区的不可用都是不可接受的，而且让到达分区的客户端有所进展是非常重要的。在那种情况下，每个分区都被分配一组新的节点以接受数据。当分区分割恢复后，合并操作会被执行。在亚马逊，购物车使用这种“write-always”的系统；分区出现时，允许顾客继续把物品放到购物车里面，即便是之前的购物车存储在其他分区。当分区恢复后，购物系统帮助存储系统合并各个分区的购物车。

### 亚马逊的Dynamo

A system that has brought all of these properties under explicit control of the application architecture is [Amazon's Dynamo](http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html), a key-value storage system that is used internally in many services that make up the Amazon e-commerce platform, as well as Amazon's Web Services. One of the design goals of Dynamo is to allow the application service owner who creates an instance of the Dynamo storage system—which commonly spans multiple data centers—to make the trade-offs between consistency, durability, availability, and performance at a certain cost point.3


### 总结

Data inconsistency in large-scale reliable distributed systems has to be tolerated for two reasons: improving read and write performance under highly concurrent conditions; and handling partition cases where a majority model would render part of the system unavailable even though the nodes are up and running.

Whether or not inconsistencies are acceptable depends on the client application. In all cases the developer needs to be aware that consistency guarantees are provided by the storage systems and need to be taken into account when developing applications. There are a number of practical improvements to the eventual consistency model, such as session-level consistency and monotonic reads, which provide better tools for the developer. Many times the application is capable of handling the eventual consistency guarantees of the storage system without any problem. A specific popular case is a Web site in which we can have the notion of user-perceived consistency. In this scenario the inconsistency window needs to be smaller than the time expected for the customer to return for the next page load. This allows for updates to propagate through the system before the next read is expected.

The goal of this article is to raise awareness about the complexity of engineering systems that need to operate at a global scale and that require careful tuning to ensure that they can deliver the durability, availability, and performance that their applications require. One of the tools the system designer has is the length of the consistency window, during which the clients of the systems are possibly exposed to the realities of large-scale systems engineering.

原文链接：<http://www.allthingsdistributed.com/2008/12/eventually_consistent.html>
