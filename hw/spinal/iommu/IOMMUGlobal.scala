package iommu

object IOMMUGlobal {
  val CAUSE_ENUM = new {
    val INSTRUCTION_ACCESS_FAULT = 1
    val READ_ADDRESS_MISALIGNED = 4
    val READ_ACCESS_FAULT = 5
    val WRITE_AMO_ADDRESS_MISALIGNED = 6
    val WRITE_AMO_ACCESS_FAULT = 7
    val INSTRUCTION_PAGE_FAULT = 12
    val READ_PAGE_FAULT = 13
    val WRITE_AMO_PAGE_FAULT = 15
    val INSTRUCTION_GUEST_PAGE_FAULT = 20
    val READ_GUEST_PAGE_FAULT = 21
    val WRITE_AMO_GUEST_PAGE_FAULT = 23
    val ALL_INBOUND_TRANSACTIONS_DISALLOWED = 256
    val DDT_ENTRY_LOAD_ACCESS_FAULT = 257
    val DDT_ENTRY_NOT_VALID = 258
    val DDT_ENTRY_MISCONFIGURED = 259
    val TRANSACTION_TYPE_DISALLOWED = 260
    val MSI_PTE_LOAD_ACCESS_FAULT = 261
    val MSI_PTE_NOT_VALID = 262
    val MSI_PTE_MISCONFIGURED = 263
    val MRIF_ACCESS_FAULT = 264
    val PDT_ENTRY_LOAD_ACCESS_FAULT = 265
    val PDT_ENTRY_NOT_VALID = 266
    val PDT_ENTRY_MISCONFIGURED = 267
    val DDT_DATA_CORRUPTION = 268
    val PDT_DATA_CORRUPTION = 269
    val MSI_PT_DATA_CORRUPTION = 270
    val MSI_MRIF_DATA_CORRUPTION = 271
    val INTERNAL_DATA_PATH_ERROR = 272
    val IOMMU_MSI_WRITE_ACCESS_FAULT = 273
    val FIRST_SECOND_STAGE_PT_DATA_CORRUPTION = 274
  }

  val MAPPING = new {
    val capabilitiesOffset = 0x0000
    val fctlOffset = 0x0008
    val ddtpOffset = 0x0010
    val cqbOffset = 0x0018
    val cqhOffset = 0x0020
    val cqtOffset = 0x0024
    val fqbOffset = 0x0028
    val fqhOffset = 0x0030
    val fqtOffset = 0x0034
    val pqbOffset = 0x0038
    val pqhOffset = 0x0040
    val pqtOffset = 0x0044
    val cqcsrOffset = 0x0048
    val fqcsrOffset = 0x004c
    val pqcsrOffset = 0x0050
    val ipsrOffset = 0x0054
    val iocountovfOffset = 0x0058
    val iocountinhOffset = 0x005c
    val iohpmcyclesOffset = 0x0060
    val iohpmctr1Offset = 0x0068
    val iohpmevt1Offset = 0x0160
    val tr_req_iovaOffset = 0x0258
    val tr_req_ctlOffset = 0x0260
    val tr_responseOffset = 0x0268
    val iommuQosidOffset = 0x0270
    val icvecOffset = 0x02f8
    val msiCfgTableOffset = 0x0300
  }
}

