.. role:: raw-html-m2r(raw)
   :format: html

.. _aplic_mapper:

RISC-V Advanced Interrupt Architecture Support
==============================================

Support Matrix

+------------+---------------------------------+---------------+
| Part       | Features                        | State         |
+============+=================================+===============+
| APLIC      | Interrupt Delegation            | Supported     |
+            +---------------------------------+---------------+
|            | Direct delivery mode            | Supported     |
+            +---------------------------------+---------------+
|            | MSI delivery mode               | Supported     |
+            +---------------------------------+---------------+
|            | Endian Support                  | Not Supported |
+------------+---------------------------------+---------------+
| IMSIC      |                                 | Supported     |
+------------+---------------------------------+---------------+

The APlicMapper defines the register generation and access for a APLIC (Advanced Platform Level Interrupt Controller.

``APlicMapper.apply``
--------------------

``(bus: BusSlaveFactory)(aplic: APlic)``

args for APlicMapper:

* **bus**: bus to which this ctrl is attached
* **aplic**: instance of the generated aplic

It follows the interface given by riscv: https://github.com/riscv/riscv-aia

``APlic.apply``
---------------

``(p: APlicDomainParam, sourceIds: Seq[Int], hartIds: Seq[Int], childInfos: Seq[APlicChildInfo])``

args for APlic:

* ``p``: a generation parameter for APLIC
* ``sourceIds``: a sequence of the source interrupt identient
* ``hartIds``: a sequence of the hart id, which is includes is this APLIC
* ``childInfos``: a sequence of delegation information of the APLIC

As the generation params are complex, some helper function and configuration is introduced:

.. _aplic-configuration:

Configurations:
---------------

* ``APlicGenParam.full`` includes both direct mode and MSI mode support, except some
features used for testing
* ``APlicGenParam.direct`` only includes direct mode
* ``APlicGenParam.msi`` only includes MSI mode
* ``APlicGenParam.test`` includes all features in ``full``, with some testing features (such as ``iforce`` register)

Mode Control
------------

* ``APlicDomainParam.root()`` indicates the configuration is used for the root domain
* ``APlicDomainParam.M()`` indicates the configuration is used for a M-mode domain
* ``APlicDomainParam.S()`` indicates the configuration is used for a S-mode domain

``(genParam: APlicGenParam)``

args for mode control functions:

* ``genParam``: the configuration used for generating APLIC, see Configurations.

These helpers should be used in combine. For example, `APlicDomainParam.root(APlicGenParam.full)` means the APLIC is the root domain, with all necessary features enabled.


``ImsicTrigger.apply``
----------------------

The ImsicTrigger defines the register generation for IMSIC, and provides an trigger interface for the ISA part.

``(bus: BusSlaveFactory, mapping: ImsicMapping)(infos: Seq[ImsicFileInfo])``

args for ImsicTrigger:

* ``bus``: bus to which this ctrl is attached
* ``mapping``:  mapping configuration for the IMSIC register layout
* ``infos``: a sequence of the IMSIC file information

``ImsicTrigger.apply``
----------------------
